{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

{-| A set of rules for basic charts
-}
module EasyBI.Vis.Rules
  ( assignColor
  , barChart
  , lineChart
  , makeChart
  , scatterplot
  ) where

import Control.Applicative       (Alternative (..))
import Control.Monad.Logic.Class ((>>-))
import EasyBI.Vis.Types          (Archetype (..), Encoding, Mark (..),
                                  Measurement (..), Relation, Rule, archetype,
                                  colorChannel, fieldPositionChannel,
                                  markChannel, measurement, positionX,
                                  positionY)
import EasyBI.Vis.Utils          (choose, choose2, choose3, setOrFail')

{-| Visualise data in a bar chart
-}
barChart :: forall f. (Eq f, Relation f) => Rule f
barChart dims = horizontalBarChart dims <|> verticalBarChart dims

horizontalBarChart :: forall f. (Eq f, Relation f) => Rule f
horizontalBarChart dims = do
  setOrFail' @(Encoding f) archetype HorizontalBarChart
  setOrFail' @(Encoding f) markChannel Bar
  let isX x = measurement x == Nominal || measurement x == Ordinal
      isY y = measurement y == Quantitative
  choose2 (isX, isY) dims >>- \((nom, quanti), remaining) -> do
    setOrFail' @(Encoding f) positionX (fieldPositionChannel quanti)
    setOrFail' @(Encoding f) positionY (fieldPositionChannel nom)
    pure remaining

verticalBarChart :: forall f. (Eq f, Relation f) => Rule f
verticalBarChart dims = do
  setOrFail' @(Encoding f) archetype VerticalBarChart
  setOrFail' @(Encoding f) markChannel Bar
  let isX x = measurement x == Nominal || measurement x == Ordinal
      isY y = measurement y == Quantitative
  choose2 (isX, isY) dims >>- \((nom, quanti), remaining) -> do
    setOrFail' @(Encoding f) positionX (fieldPositionChannel nom)
    setOrFail' @(Encoding f) positionY (fieldPositionChannel quanti)
    pure remaining

{-| Scatterplot for two quantitative dimensions
-}
scatterplot :: forall f. (Relation f, Eq f) => Rule f
scatterplot dims = do
  setOrFail' @(Encoding f) archetype Scatterplot
  setOrFail' @(Encoding f) markChannel Point
  let isX x = measurement x == Quantitative
      isY y = measurement y == Quantitative
  choose2 (isX, isY) dims >>- \((x, y), remaining) -> do
    setOrFail' @(Encoding f) positionX (fieldPositionChannel x)
    setOrFail' @(Encoding f) positionY (fieldPositionChannel y)
    pure remaining

{-| Line chart for temporal data
-}
lineChart :: forall f. (Relation f, Eq f) => Rule f
lineChart dims = do
  setOrFail' @(Encoding f) archetype Linechart
  setOrFail' @(Encoding f) markChannel Line
  let isX x = measurement x `elem` [TemporalAbs, TemporalRel]
      isY y = measurement y `elem` [Quantitative, Ordinal]
  choose2 (isX, isY) dims >>- \((x, y), remaining) -> do
    setOrFail' @(Encoding f) positionX (fieldPositionChannel x)
    setOrFail' @(Encoding f) positionY (fieldPositionChannel y)
    pure remaining

{-| Heatmap for two nominal / ordinal dimensions with a quantitative measure
-}
heatmap :: forall f. (Relation f, Eq f) => Rule f
heatmap dims = do
  setOrFail' @(Encoding f) archetype Heatmap
  setOrFail' @(Encoding f) markChannel Rect
  let isX x = measurement x == Nominal || measurement x == Ordinal
      isY y = measurement y == Nominal || measurement y == Ordinal
      isC c = measurement c == Quantitative
  choose3 (isX, isY, isC) dims >>- \((x, y, c), rest) -> do
    setOrFail' @(Encoding f) positionX (fieldPositionChannel x)
    setOrFail' @(Encoding f) positionY (fieldPositionChannel y)
    setOrFail' @(Encoding f) colorChannel c
    pure rest

{-| Display a dimension with a color scale
-}
assignColor :: forall f. (Relation f, Eq f) => Rule f
assignColor dims = choose dims >>- \(c, rest) -> do
  if measurement c `elem` [Quantitative, Nominal, Ordinal]
    then do
      setOrFail' @(Encoding f) colorChannel c
      pure rest
    else pure dims

{-| Display the data in a chart, optionally using the color dimension
(for groups, etc)
-}
makeChart :: (Relation f, Eq f) => Rule f
makeChart dims = do
  let abc = barChart dims <|> scatterplot dims <|> lineChart dims >>- \rest -> do
        case rest of
          [] -> pure []
          _  -> assignColor rest
  abc <|> heatmap dims
