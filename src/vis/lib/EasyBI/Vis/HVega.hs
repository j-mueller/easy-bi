{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
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

import Data.Aeson             qualified as JSON
import Data.Text.Lazy         qualified as TL
import EasyBI.Vis.Charts      (Chart (..), HeatmapSpec (..),
                               HorizontalBarChartSpec (..), LineChartSpec (..),
                               LineChartSpec2Axis (..), ScatterplotSpec (..),
                               VerticalBarChartSpec (..))
import EasyBI.Vis.Types       (Measurement (..), Relation (..), fieldName,
                               measurement)
import Graphics.Vega.VegaLite (BuildEncodingSpecs, Position (..), PropertySpec,
                               VegaLite)
import Graphics.Vega.VegaLite qualified as VL

{-| Convert an encoding to a 'VegaLite' specification
-}
vegaLite :: Relation f => Chart f -> VegaLite
vegaLite = VL.toVegaLite . chartProperties

toHtml :: Relation f => Chart f -> TL.Text
toHtml = VL.toHtml . vegaLite

toHtmlFile :: Relation f => FilePath -> Chart f -> IO ()
toHtmlFile fp enc = VL.toHtmlFile fp (vegaLite enc)

toJSON :: Relation f => Chart f -> JSON.Value
toJSON = VL.fromVL . vegaLite

nominal :: Relation a => Position -> a -> BuildEncodingSpecs
nominal p f = VL.position p [VL.PName (fieldName f), VL.PmType (mkMeasurement $ measurement f), VL.PTitle (fieldLabel f)]

quantitative :: Relation a => Position -> a -> BuildEncodingSpecs
quantitative p f = VL.position p [VL.PName (fieldName f), VL.PmType (mkMeasurement $ measurement f), VL.PTitle (fieldLabel f), VL.PScale [VL.SType VL.ScLinear]]

temporal :: Relation a => Position -> a -> BuildEncodingSpecs
temporal p f = VL.position p [VL.PName (fieldName f), VL.PmType VL.Temporal, VL.PTitle (fieldLabel f), VL.PScale [VL.SType VL.ScLinear]]

chartProperties :: Relation f => Chart f -> [PropertySpec]
chartProperties = \case
  VerticalBarChart VerticalBarChartSpec{vbcXAxis, vbcYAxis, vbcColor} ->
    [ VL.encoding
      $ nominal      X vbcXAxis
      $ quantitative Y vbcYAxis
      $ color          vbcColor
      $ []
    , VL.mark VL.Bar []
    ]
  HorizontalBarChart HorizontalBarChartSpec{hbcXAxis, hbcYAxis, hbcColor} ->
    [ VL.encoding
      $ nominal      Y hbcYAxis
      $ quantitative X hbcXAxis
      $ color          hbcColor
      $ []
    , VL.mark VL.Bar []
    ]
  Scatterplot ScatterplotSpec{spX, spY, spColor} ->
    [ VL.encoding
      $ quantitative X spX
      $ quantitative Y spY
      $ color spColor
      $ []
    , VL.mark VL.Point []
    ]
  LineChart LineChartSpec{lcX, lcY, lcColor} ->
    [ VL.encoding
      $ temporal X lcX
      $ quantitative Y lcY
      $ color lcColor
      $ []
    , VL.mark VL.Line []
    ]
  Heatmap HeatmapSpec{hsX, hsY, hsMeasure} ->
    [ VL.encoding
      $ nominal X hsX
      $ nominal Y hsY
      $ color (Just hsMeasure)
      $ []
    , VL.mark VL.Rect []
    ]
  LineChart2Axis LineChartSpec2Axis{lc2X, lc2Y, lc2Y2} ->
    [ VL.encoding $ nominal X lc2X []
    , VL.layer
        [ VL.asSpec [VL.encoding $ quantitative Y lc2Y [], VL.mark VL.Line []]
        , VL.asSpec [VL.encoding $ quantitative Y lc2Y2 [], VL.mark VL.Line []]
        ]
    , VL.resolve $ VL.resolution (VL.RScale [(VL.ChY, VL.Independent)]) []
    ]

color :: Relation f => Maybe f -> BuildEncodingSpecs
color Nothing  = id
color (Just k) = VL.color [ VL.MName (fieldName k)]

mkMeasurement :: Measurement -> VL.Measurement
mkMeasurement = \case
  Nominal      -> VL.Nominal
  Ordinal      -> VL.Ordinal
  Quantitative -> VL.Quantitative
  TemporalRel  -> VL.Nominal
  TemporalAbs  -> VL.Temporal
