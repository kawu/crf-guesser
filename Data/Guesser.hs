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
    [ Ox.prefix 1 $ orth k
    , Ox.prefix 2 $ orth k
    -- , Ox.prefix 3 $ orth k
    , Ox.suffix 1 $ orth k
    , Ox.suffix 2 $ orth k
    -- , Ox.suffix 3 $ orth k
    , Ox.known sent k
    -- , shape k
    , Ox.join "-" (Ox.isBeg sent k) (packedShape k) ]
  where
    orth = Ox.lowerOrth sent
    shape = Ox.shape . orth
    packedShape = Ox.pack . shape

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
        [ if null (M.interps word)
            then word { M.interps = map (M.Interp "None") choice }
            else word
        | (word, choice) <- zip sent choices ]

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
    let readTrain = map schematize <$> readData trainPath
    let readEval  = map schematize <$> readData evalPath

--     lbSet <- U.fromList . S.toList . S.fromList . collectLbs
-- 	 <$> readData trainPath

    -- | TODO: There should be stronger consistency between CRF
    -- codec and CRF model...
    let ign = Tag "unknown" Map.empty
    codec <- Codec.mkCodec ign . concat <$> readTrain

    lbSet <- unknownLbs . map (CRF.encodeSent codec) <$> readTrain
    putStrLn $ show (U.length lbSet) ++ " tags assigned to unknown words:"
    print (U.toList lbSet)

    trainData <- V.fromList <$> map (CRF.encodeSent' lbSet codec) <$> readTrain
    evalData  <- V.fromList <$> map (CRF.encodeSent' lbSet codec) <$>
        if null evalPath
            then return []
            else readEval

    lbNum <- U.length . hiddenLbs . map (CRF.encodeSent codec) <$> readTrain
    let fts = Ft.presentOFeats trainData
           ++ Ft.presentSFeats trainData
           ++ Ft.presentTFeats trainData
    let crf = CRF.mkModel (lbNum+1) fts
    crf' <- SGD.sgd sgdArgs trainData evalData crf
    return $ Guesser crf' codec lbSet

unknownLbs :: [(CRF.Rs, CRF.Ys)] -> U.Vector CRF.Lb
unknownLbs =
    U.fromList . nub . concatMap sentLbs
  where
    sentLbs (rs, ys) = concat
        [ if null (CRF.lbs r)
            then map fst (CRF.choice y)
            else []
        | (r, y) <- zip (V.toList rs) (V.toList ys) ]
    nub = S.toList . S.fromList

hiddenLbs :: [(CRF.Rs, CRF.Ys)] -> U.Vector CRF.Lb
hiddenLbs =
    U.fromList . nub . concatMap sentLbs
  where
    sentLbs (rs, ys) = concat
        [ map fst (CRF.choice y) ++ CRF.lbs r
        | (r, y) <- zip (V.toList rs) (V.toList ys) ]
    nub = S.toList . S.fromList

-- collectLbs :: [M.SentMlt] -> [M.Tag]
-- collectLbs =
--     concatMap sentLbs
--   where
--     sentLbs sent = concat
--         [ map (M.tag . fst) choice ++
--           map M.tag (M.interps word)
--         | (word, choice) <- sent ]

schematize :: M.SentMlt -> [Word.Word L.Text M.Tag]
schematize sent = map (fmap M.tag)
    [ Word.Word obs (M.interps word) choice
    | (obs, word, choice) <- zip3 schemed xs ys ]
  where
    schemed = Ox.runSchema schema (V.fromList xs)
    xs = map fst sent
    ys = map snd sent

schematize' :: M.Sent -> [Word.Word L.Text M.Tag]
schematize' sent = schematize [(word, []) | word <- sent]
