{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Guesser
( Guesser (..)
, guess
, learn
) where

import Control.Applicative ((<$>), (<*>))
import Data.Binary (Binary, put, get)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.Map as Map
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import Data.ListLike.Vector

import qualified Data.CRF.Codec as Codec
import qualified Data.CRF.Word as CRF
import qualified Data.CRF.R as CRF
import qualified Data.RCRF2 as CRF
import qualified Data.CRF.RCRF2.Model as CRF -- ^ TODO: delete it! 

import qualified Data.CRF.FeatSel.Present as Ft
import qualified Data.CRF.FeatSel.Hidden as Ft

import qualified Data.Morphosyntax as M
import Data.Morphosyntax.Tagset (Tag(Tag))
import Text.Morphosyntax.Tagset (parseTagset)
import Text.Morphosyntax.Plain (parsePlain)

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

guess :: Guesser -> M.Sent -> M.Sent
guess Guesser{..} sent =
    apply choices sent
  where
    encoded = fst $ CRF.encodeSent' labels codec $ schematize' sent
    choices = map (CRF.decodeL codec) $ CRF.tag crf encoded
    apply choices sent =
        [ if null (M.interps word)
            then word { M.interps = [M.Interp "None" disamb] }
            else word
        | (word, disamb) <- zip sent choices ]

-- | TODO: Abstract over format type.
learn
    :: SGD.SgdArgs      -- ^ SGD parameters 
    -> FilePath         -- ^ Tagset file (plain format)
    -> FilePath         -- ^ Train file  (plain format)
    -> FilePath         -- ^ Eval file (maybe null) 
    -> IO Guesser
learn sgdArgs tagsetPath trainPath evalPath = do

    tagset <- parseTagset tagsetPath <$> readFile tagsetPath

    let readData path = parsePlain tagset <$> L.readFile path
    let readTrain = map schematize <$> readData trainPath
    let readEval  = map schematize <$> readData evalPath

    lbNum <- S.size . S.fromList . collectLbs <$> readData trainPath
    let lbSet  = U.fromList [0 .. lbNum - 1]

    -- | FIXME: should be "unknown" or "ign" used as an unknown tag?
    -- let ign = Tag "unknown" Map.empty
    -- codec <- Codec.mkCodec ign . concat <$> readTrain
    codec <- Codec.mkCodec . concat <$> readTrain

    trainData <- V.fromList <$> map (CRF.encodeSent' lbSet codec) <$> readTrain
    evalData  <- V.fromList <$> map (CRF.encodeSent' lbSet codec) <$>
        if null evalPath
            then return []
            else readEval

    let fts = Ft.presentOFeats trainData
           ++ Ft.presentSFeats trainData
           ++ Ft.presentTFeats trainData
    let crf = CRF.mkModel lbNum fts
    crf' <- SGD.sgd sgdArgs trainData evalData crf
    return $ Guesser crf' codec lbSet

collectLbs :: [M.SentMlt] -> [M.Tag]
collectLbs =
    concatMap sentLbs
  where
    sentLbs sent = concat
        [ map (M.tag . fst) choice ++
          map M.tag (M.interps word)
        | (word, choice) <- sent ]

schematize :: M.SentMlt -> [CRF.Word L.Text M.Tag]
schematize sent = map (fmap M.tag)
    [ CRF.Word obs (M.interps word) choice
    | (obs, word, choice) <- zip3 schemed xs ys ]
  where
    schemed = Ox.runSchema schema (V.fromList xs)
    xs = map fst sent
    ys = map snd sent

schematize' :: M.Sent -> [CRF.Word L.Text M.Tag]
schematize' sent = schematize [(word, []) | word <- sent]
