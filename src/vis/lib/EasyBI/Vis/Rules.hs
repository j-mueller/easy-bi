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
import Control.Monad             (guard)
import Control.Monad.Logic.Class ((>>-))
import EasyBI.Vis.Types          (Archetype (..), Encoding, Mark (..),
                                  Measurement (..), Relation, Rule, archetype,
                                  colorChannel, fieldPositionChannel,
                                  markChannel, measurement, positionX,
                                  positionY)
import EasyBI.Vis.Utils          (choose, setOrFail')

{-| Visualise data in a bar chart
-}
barChart :: forall f. (Eq f, Relation f) => Rule f
barChart dims = do
  setOrFail' @(Encoding f) markChannel Bar
  choose dims >>- \(x, rest) -> do
    guard $ measurement x == Nominal || measurement x == Ordinal
    choose rest >>- \(y, remaining) -> do
      guard $ measurement y == Quantitative
      let horz = do
            setOrFail' @(Encoding f) positionX (fieldPositionChannel y)
            setOrFail' @(Encoding f) positionY (fieldPositionChannel x)
            setOrFail' @(Encoding f) archetype HorizontalBarChart
          vert = do
            setOrFail' @(Encoding f) positionX (fieldPositionChannel x)
            setOrFail' @(Encoding f) positionY (fieldPositionChannel y)
            setOrFail' @(Encoding f) archetype VerticalBarChart
      horz <|> vert
      pure remaining

{-| Scatterplot for two quantitative dimensions
-}
scatterplot :: forall f. (Relation f, Eq f) => Rule f
scatterplot dims = do
  setOrFail' @(Encoding f) markChannel Point
  choose dims >>- \(x, rest) -> do
    guard $ measurement x == Quantitative
    choose rest >>- \(y, remaining) -> do
      guard $ measurement y == Quantitative
      setOrFail' @(Encoding f) positionX (fieldPositionChannel x)
      setOrFail' @(Encoding f) positionY (fieldPositionChannel y)
      setOrFail' @(Encoding f) archetype Scatterplot
      pure remaining

{-| Line chart for temporal data
-}
lineChart :: forall f. (Relation f, Eq f) => Rule f
lineChart dims = do
  setOrFail' @(Encoding f) markChannel Line
  choose dims >>- \(x, rest) -> do
    guard $ measurement x `elem` [TemporalAbs, TemporalRel]
    choose rest >>- \(y, remaining) -> do
      guard $ measurement y `elem` [Quantitative, Ordinal]
      setOrFail' @(Encoding f) positionX (fieldPositionChannel x)
      setOrFail' @(Encoding f) positionY (fieldPositionChannel y)
      setOrFail' @(Encoding f) archetype Linechart
      pure remaining

{-| Heatmap for two nominal / ordinal dimensions with a quantitative measure
-}
heatmap :: forall f. (Relation f, Eq f) => Rule f
heatmap dims = do
  setOrFail' @(Encoding f) markChannel Rect
  choose dims >>- \(x, r1) -> do
    guard $ measurement x == Nominal || measurement x == Ordinal
    choose r1 >>- \(y, rest'_) -> do
      guard $ measurement y == Nominal || measurement y == Ordinal
      choose rest'_ >>- \(c, rest) -> do
        guard $ measurement c == Quantitative
        setOrFail' @(Encoding f) colorChannel c
        setOrFail' @(Encoding f) archetype Heatmap
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
