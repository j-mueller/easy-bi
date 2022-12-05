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
  ( makeChart,
    barChart,
    assignColor,
    lineChart,
    scatterplot,
  )
where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (guard)
import           EasyBI.Vis.Types    (Encoding, Mark (..), Measurement (..),
                                      Relation, Rule, colorChannel, dependsOn,
                                      field, mark, markChannel, measurement,
                                      positionX, positionY)
import           EasyBI.Vis.Utils    (choose, setOrFail')

{-| Visualise data in a bar chart
-}
barChart :: forall f. (Eq f, Relation f) => Rule f
barChart dims = do
  setOrFail' @(Encoding f) (markChannel . mark) Bar
  (x, rest) <- choose dims
  guard $ measurement x == Nominal || measurement x == Ordinal
  (y, remaining) <- choose rest
  guard $ measurement y == Quantitative
  guard $ y `dependsOn` x
  let horz = do
        setOrFail' @(Encoding f) (positionX . field) y
        setOrFail' @(Encoding f) (positionY . field) x
      vert = do
        setOrFail' @(Encoding f) (positionX . field) x
        setOrFail' @(Encoding f) (positionY . field) y
  horz <|> vert
  pure remaining

{-| Scatterplot for two quantitative dimensions
-}
scatterplot :: forall f. (Relation f, Eq f) => Rule f
scatterplot dims = do
  setOrFail' @(Encoding f) (markChannel . mark) Point
  (x, rest) <- choose dims
  guard $ measurement x == Quantitative
  (y, remaining) <- choose rest
  guard $ measurement y == Quantitative
  setOrFail' @(Encoding f) (positionX . field) x
  setOrFail' @(Encoding f) (positionY . field) y
  pure remaining

{-| Line chart for temporal data
-}
lineChart :: forall f. (Relation f, Eq f) => Rule f
lineChart dims = do
  setOrFail' @(Encoding f) (markChannel . mark) Line
  (x, rest) <- choose dims
  guard $ measurement x `elem` [TemporalAbs, TemporalRel]
  (y, remaining) <- choose rest
  guard $ measurement y `elem` [Quantitative, Ordinal]
  setOrFail' @(Encoding f) (positionX . field) x
  setOrFail' @(Encoding f) (positionY . field) y
  pure remaining

{-| Heatmap for two nominal / ordinal dimensions with a quantitative measure
-}
heatmap :: forall f. (Relation f, Eq f) => Rule f
heatmap dims = do
  setOrFail' @(Encoding f) (markChannel . mark) Rect
  (x, _) <- choose dims
  guard $ measurement x == Nominal || measurement x == Ordinal
  (y, rest'_) <- choose dims
  guard $ measurement y == Nominal || measurement x == Ordinal
  (c, rest) <- choose rest'_
  guard $ measurement c == Quantitative
  setOrFail' @(Encoding f) (colorChannel . field) c
  pure rest

{-| Display a dimension with a color scale
-}
assignColor :: forall f. (Relation f, Eq f) => Rule f
assignColor dims = do
  (c, rest) <- choose dims
  if measurement c `elem` [Quantitative, Nominal, Ordinal]
    then do
      setOrFail' @(Encoding f) (colorChannel . field) c
      pure rest
    else pure dims

{-| Display the data in a chart, optionally using the color dimension
(for groups, etc)
-}
makeChart :: (Relation f, Eq f) => Rule f
makeChart dims = do
  let abc = do
        rest <- barChart dims <|> scatterplot dims <|> lineChart dims
        case rest of
          [] -> pure []
          _  -> assignColor rest
  abc <|> heatmap dims
