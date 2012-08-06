{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Guesser
( Guesser (..)
, guess
, tagFile
, learn
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Binary (Binary, put, get, decodeFile)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.ListLike.Vector

import qualified Data.CRF.Codec as Codec
import qualified Data.CRF.Word as Word
import qualified Data.CRF.R as CRF
import qualified Data.CRF.Y as CRF
import qualified Data.RCRF2 as CRF
import qualified Data.CRF.RCRF2.Model as CRF -- ^ TODO: delete it! 

import qualified Data.CRF.FeatSel.Present as Ft
import qualified Data.CRF.FeatSel.Hidden as Ft

import qualified Data.Morphosyntax as M
import Data.Morphosyntax.Tagset (Tag(Tag))
import Text.Morphosyntax.Tagset (parseTagset)
import Text.Morphosyntax.Plain (parsePlain, showPlain)

import qualified Data.Schema as Ox
import qualified Data.Feature as Ox

import qualified SGD as SGD

type Schema = Ox.Schema M.Word

-- | At the moment only this schema can be used.
-- TODO: Move to Data.Guesser.Schema module.
schema :: Schema
schema sent = \k ->
    [ Ox.prefix 1 $ lowOrth `at` k
    , Ox.prefix 2 $ lowOrth `at` k
    , Ox.suffix 1 $ lowOrth `at` k
    , Ox.suffix 2 $ lowOrth `at` k
    , known `at` k
    , Ox.join "-" (isBeg k) (packedShape `at` k) ]
  where
    at  = Ox.at sent
    orth  = (:[]) . M.orth
    lowOrth = map L.toLower . orth
    shape = Ox.shape . orth
    packedShape = Ox.pack . shape
    known       = boolF . M.known
    isBeg k     = boolF (k == 0)
    boolF True  = ["T"]
    boolF False = ["F"]

data Guesser = Guesser
    { crf       :: CRF.Model
    , codec     :: CRF.Codec L.Text M.Tag
    -- | Vector of potential labels equall to U.fromList (CRF.lbSet crf).
    , labels    :: U.Vector CRF.Lb }
    -- | TODO: Think of a way to dynamically compile a schema.
    -- , schema    :: Schema }

-- | Already defined in Data.Morphosyntax.Tagset!
-- FIXME: make separate package for this instance.
-- instance Binary L.Text where
--     put = put . L.encodeUtf8
--     get = L.decodeUtf8 <$> get

instance Binary Guesser where
    put Guesser{..} = sequence_ [put crf, put codec, put labels]
    get = Guesser <$> get <*> get <*> get

guess :: Int -> Guesser -> M.Sent -> M.Sent
guess k Guesser{..} sent =
    apply choices sent
  where
    encoded = fst $ CRF.encodeSent' labels codec $ schematize' sent
    decodeChoice = map (CRF.decodeL codec . fst)
    choices = map decodeChoice $ CRF.tagK k crf encoded
    apply choices sent =
        [ if (not . M.known) word
            then word { M.interps = map (M.Interp "None") choice }
            else word
        | (word, choice) <- zip sent choices ]

-- | FIXME: Tagset should be stored together with a guesser.
tagFile
    :: Int              -- ^ Guesser argument
    -> Guesser          -- ^ Guesser itself
    -> FilePath         -- ^ Tagset file
    -> FilePath         -- ^ File to tag (plain format)
    -> IO ()
tagFile k guesser tagsetPath inPath = do
    tagset <- parseTagset tagsetPath <$> readFile tagsetPath
    input <- parsePlain tagset <$> L.readFile inPath
    L.putStr $ showPlain tagset $ map doGuess input
  where
    doGuess sent =
        let xs = map fst sent
            ys = map snd sent
        in  zip (guess k guesser xs) ys

-- | TODO: Abstract over format type.
learn
    :: SGD.SgdArgs      -- ^ SGD parameters 
    -> FilePath         -- ^ Tagset file
    -> FilePath         -- ^ Train file  (plain format)
    -> FilePath         -- ^ Eval file (maybe null) 
    -> IO Guesser
learn sgdArgs tagsetPath trainPath evalPath = do

    tagset <- parseTagset tagsetPath <$> readFile tagsetPath

    let readData path = parsePlain tagset <$> L.readFile path
    let schemaTrain = map schematize <$> readData trainPath

    -- | TODO: There should be stronger consistency between CRF
    -- codec and CRF model...
    let ign = Tag "unknown" Map.empty
    codec <- Codec.mkCodec ign . concat <$> schemaTrain
    putStrLn $ "labels number: " ++ show (Codec.lbNum codec)
    putStrLn $ "observations number: " ++ show (Codec.obNum codec)

    let nub = S.toList . S.fromList
    lbSet' <- nub . concatMap unknownLbs <$> readData trainPath
    let lbSet = U.fromList $ sort $ map (fromJust . Codec.encodeL codec) lbSet'
    putStrLn $ show (U.length lbSet) ++ " tags assigned to unknown words:"
    print (U.toList lbSet)

    trainData <- V.fromList <$> map (CRF.encodeSent' lbSet codec) <$>
        schemaTrain
    evalData  <- V.fromList <$> map (CRF.encodeSent' lbSet codec) <$>
        if null evalPath
            then return []
            else map schematize <$> readData evalPath

    -- | FIXME: Is there a need to compute this again (see Codec.lbNum above).
    lbNum <- length . nub . concatMap hiddenLbs <$> readData trainPath
    let fts = Ft.presentOFeats trainData
           ++ Ft.presentSFeats trainData
           ++ Ft.presentTFeats trainData
    let crf = CRF.mkModel (lbNum+1) fts
    crf' <- SGD.sgd sgdArgs trainData evalData crf
    return $ Guesser crf' codec lbSet

unknownLbs :: M.SentMlt -> [M.Tag]
unknownLbs sent = concat
    [ if M.known word
        then []
        else map (M.tag . fst) choice
    | (word, choice) <- sent ]

hiddenLbs :: M.SentMlt -> [M.Tag]
hiddenLbs sent = map M.tag $ concat
    [ map fst choice ++ M.interps word
    | (word, choice) <- sent ]

schematize :: M.SentMlt -> [Word.Word L.Text M.Tag]
schematize sent = map (Word.rmDups . fmap M.tag)
    [ Word.Word obs (lbs word) choice
    | (obs, word, choice) <- zip3 schemed xs ys ]
  where
    schemed = Ox.runSchema schema (V.fromList xs)
    xs = map fst sent
    ys = map snd sent
    lbs word
        | M.known word = M.interps word
        | otherwise = []

schematize' :: M.Sent -> [Word.Word L.Text M.Tag]
schematize' sent = schematize [(word, []) | word <- sent]
