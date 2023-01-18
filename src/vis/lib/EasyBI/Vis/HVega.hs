{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module EasyBI.Vis.HVega
  ( vegaLite
    -- * Helper / utility
  , toHtml
  , toHtmlFile
  , toJSON
  ) where

import Control.Lens           (_Just, to, (^.), (^..))
import Control.Monad.Writer   (MonadWriter, runWriter, runWriterT, tell)
import Data.Aeson             qualified as JSON
import Data.Foldable          (forM_, toList, traverse_)
import Data.Sequence          (Seq)
import Data.Sequence          qualified as Seq
import Data.Text.Lazy         qualified as TL
import EasyBI.Vis.Types       (Encoding, Mark (..), Measurement (..),
                               PositionChannel, Relation (..), ScaleTp (..),
                               colorChannel, field, fieldName, markChannel,
                               measurement, positionX, positionY, scale,
                               scaleTp, title)
import Graphics.Vega.VegaLite (BuildEncodingSpecs, Position (..), PropertySpec,
                               VegaLite)
import Graphics.Vega.VegaLite qualified as VL

{-| Convert an encoding to a 'VegaLite' specification
-}
vegaLite :: Relation f => Encoding f -> VegaLite
vegaLite enc =
  let (_, specs) = runWriter (writeSpecs enc)
  in (VL.toVegaLite $ toList specs)

toHtml :: Relation f => Encoding f -> TL.Text
toHtml = VL.toHtml . vegaLite

toHtmlFile :: (Relation f) => FilePath -> Encoding f -> IO ()
toHtmlFile fp enc = VL.toHtmlFile fp (vegaLite enc)

toJSON :: Relation f => Encoding f -> JSON.Value
toJSON = VL.fromVL . vegaLite

writeSpecs ::
  forall f m.
  ( Relation f
  , MonadWriter (Seq PropertySpec) m
  ) =>
  Encoding f ->
  m ()
writeSpecs enc = do
  (_, encoding) <- runWriterT $ do
    traverse_ (writePositionChannel X) (enc ^. positionX)
    traverse_ (writePositionChannel Y) (enc ^. positionY)
    writeColorChannel (enc ^. colorChannel)
  tell $ Seq.singleton $ VL.encoding (encoding [])
  writeMarkChannel (enc ^. markChannel)

writePositionChannel ::
  forall f m.
  (Relation f, MonadWriter BuildEncodingSpecs m) =>
  Position ->
  PositionChannel f ->
  m ()
writePositionChannel pos ch =
  tell $ VL.position pos $
    (ch ^.. field . to fieldName . to VL.PName)
      <> (ch ^.. field . to measurement . to mkMeasurement . to VL.PmType)
      <> (ch ^.. title . to VL.PTitle)
      <> (ch ^.. scale . scaleTp . _Just . to (VL.PScale . mkScale))

writeColorChannel ::
  forall f m.
  (Relation f, MonadWriter BuildEncodingSpecs m) =>
  Maybe f ->
  m ()
writeColorChannel ch =
  tell $ VL.color $
    (ch ^.. _Just . to fieldName . to VL.MName)
      <> (ch ^.. _Just . to (VL.MmType . mkMeasurement . measurement))

writeMarkChannel ::
  forall m.
  (MonadWriter (Seq PropertySpec) m) =>
  Maybe Mark ->
  m ()
writeMarkChannel ch = forM_ ch $ \mk -> do
  tell $ Seq.singleton $ VL.mark (mkMark mk) []

mkScale :: ScaleTp -> [VL.ScaleProperty]
mkScale = \case
  SLinear   -> [VL.SType VL.ScLinear]
  SLog      -> [VL.SType VL.ScLog]
  SPow      -> [VL.SType VL.ScPow]
  STime     -> [VL.SType VL.ScTime]
  SUtc      -> [VL.SType VL.ScUtc]
  SQuantile -> [VL.SType VL.ScQuantile]
  SOrdinal  -> [VL.SType VL.ScOrdinal]

mkMeasurement :: Measurement -> VL.Measurement
mkMeasurement = \case
  Nominal      -> VL.Nominal
  Ordinal      -> VL.Ordinal
  Quantitative -> VL.Quantitative
  TemporalRel  -> VL.Quantitative
  TemporalAbs  -> VL.Temporal

mkMark :: Mark -> VL.Mark
mkMark = \case
  Bar   -> VL.Bar
  Point -> VL.Point
  Line  -> VL.Line
  Rect  -> VL.Rect
