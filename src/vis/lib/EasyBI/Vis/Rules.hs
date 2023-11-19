{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

{-| A set of rules for basic charts
-}
module EasyBI.Vis.Rules
  ( horizontalBarChart
  , linechart
  , linechart2
  , makeChart
  , scatterplot
  , verticalBarChart
    -- * Utility functions
  , isQuantitative
  , isTemporal
  ) where

import Control.Applicative       (Alternative (..))
import Control.Monad             (guard)
import Control.Monad.Logic.Class (MonadLogic)
import Control.Monad.Reader      (MonadReader (..), asks)
import Data.Bifunctor            (Bifunctor (..))
import Data.Functor              (($>))
import Data.Set                  qualified as Set
import EasyBI.Vis.Charts         (HeatmapSpec (..), HorizontalBarChartSpec (..),
                                  LineChartSpec (..), LineChartSpec2Axis (..),
                                  ScatterplotSpec (..),
                                  VerticalBarChartSpec (..))
import EasyBI.Vis.Charts         qualified as Charts
import EasyBI.Vis.Types          (Archetype (..), Mark (..), Measurement (..),
                                  Relation, Rule, Selections (..), measurement)
import EasyBI.Vis.Utils          (choose1)

barChartDims :: [Measurement]
barChartDims = [Nominal, Ordinal, TemporalRel]

isBarChartDim :: Measurement -> Bool
isBarChartDim m = m `Set.member` (Set.fromList barChartDims)

isQuantitative :: Measurement -> Bool
isQuantitative = \case
  Quantitative -> True
  _            -> False

isTemporal :: Measurement -> Bool
isTemporal = \case
  TemporalAbs -> True
  _           -> False

checkMark :: (MonadReader (Selections Maybe f) m, MonadLogic m) => Mark -> m ()
checkMark mark = do
  Selections{_selectedMark} <- ask
  guard (maybe True ((==) mark) _selectedMark)

checkArchetype :: (MonadReader (Selections Maybe f) m, MonadLogic m) => Archetype -> m ()
checkArchetype tp = do
  Selections{_selectedArchetype} <- ask
  guard (maybe True ((==) tp) _selectedArchetype)

-- | Choose a field for the x axis
checkXAxisOrChoose :: (MonadReader (Selections Maybe f) m, MonadLogic m) => (f -> Bool) -> [f] -> m (f, [f])
checkXAxisOrChoose = checkOrChoose _XAxis

-- | Choose a field for the y axis
checkYAxisOrChoose :: (MonadReader (Selections Maybe f) m, MonadLogic m) => (f -> Bool) -> [f] -> m (f, [f])
checkYAxisOrChoose = checkOrChoose _YAxis

-- | Choose a field for the secondary y axis
checkYAxis2OrChoose :: (MonadReader (Selections Maybe f) m, MonadLogic m) => (f -> Bool) -> [f] -> m (f, [f])
checkYAxis2OrChoose = checkOrChoose _YAxis2

-- | Choose a field from the reader value, using the given list to find one if the value is 'Nothing'
checkOrChoose :: (MonadReader a m, MonadLogic m) => (a -> Maybe f) -> (f -> Bool) -> [f] -> m (f, [f])
checkOrChoose f condition fields = asks f >>= \case
  Nothing -> second (uncurry (++)) <$> choose1 condition fields
  Just x  -> guard (condition x) $> (x, fields)

-- | Choose a field from the reader value, using the given list to find one if the value is 'Nothing'
checkOrChooseOpt :: (MonadReader a m, MonadLogic m) => (a -> Maybe f) -> (f -> Bool) -> [f] -> m (Maybe f, [f])
checkOrChooseOpt f condition fields = asks f >>= \case
  Nothing -> pure (Nothing, fields) <|> first Just . second (uncurry (++)) <$> choose1 condition fields
  Just x  -> guard (condition x) $> (Just x, fields)

chooseColor :: (MonadReader (Selections Maybe f) m, MonadLogic m) => (f -> Bool) -> [f] -> m (Maybe f, [f])
chooseColor = checkOrChooseOpt _Color

horizontalBarChart :: forall f m. (Relation f, MonadLogic m, MonadReader (Selections Maybe f) m) => m (HorizontalBarChartSpec f)
horizontalBarChart = do
  checkMark Bar
  checkArchetype HorizontalBarChart
  (hbcXAxis, rest) <- asks _WildCards >>= checkXAxisOrChoose (isBarChartDim . measurement)
  (hbcYAxis, rest') <- checkYAxisOrChoose (isQuantitative . measurement) rest
  (hbcColor, _) <- chooseColor (isBarChartDim . measurement) rest'
  pure HorizontalBarChartSpec{hbcXAxis, hbcYAxis, hbcColor}

verticalBarChart :: forall f m. (Relation f, MonadLogic m, MonadReader (Selections Maybe f) m) => m (VerticalBarChartSpec f)
verticalBarChart = do
  checkMark Bar
  checkArchetype VerticalBarChart
  (vbcYAxis, rest) <- asks _WildCards >>= checkYAxisOrChoose (isBarChartDim . measurement)
  (vbcXAxis, rest') <- checkXAxisOrChoose (isQuantitative . measurement) rest
  (vbcColor, _) <- chooseColor (isBarChartDim . measurement) rest'
  pure VerticalBarChartSpec{vbcYAxis, vbcXAxis, vbcColor}

scatterplot :: forall f m. (Relation f, MonadLogic m, MonadReader (Selections Maybe f) m) => m (ScatterplotSpec f)
scatterplot = do
  checkMark Point
  checkArchetype Scatterplot
  (spX, rest) <- asks _WildCards >>= checkXAxisOrChoose (isQuantitative . measurement)
  (spY, rest') <- checkYAxisOrChoose (isQuantitative . measurement) rest
  (spColor, _) <- chooseColor (isBarChartDim . measurement) rest'
  pure ScatterplotSpec{spX, spY, spColor}

linechart :: forall f m. (Relation f, MonadLogic m, MonadReader (Selections Maybe f) m) => m (LineChartSpec f)
linechart = do
  checkMark Line
  checkArchetype Linechart
  (lcX, rest) <- asks _WildCards >>= checkXAxisOrChoose (isTemporal . measurement)
  (lcY, rest') <- checkYAxisOrChoose (isQuantitative . measurement) rest
  (lcColor, _) <- chooseColor (isBarChartDim . measurement) rest'
  pure LineChartSpec{lcX, lcY, lcColor}

linechart2 :: forall f m. (Relation f, MonadLogic m, MonadReader (Selections Maybe f) m) => m (LineChartSpec2Axis f)
linechart2 = do
  checkMark Line
  checkArchetype Linechart
  (lc2X, rest) <- asks _WildCards >>= checkXAxisOrChoose (isTemporal . measurement)
  (lc2Y, rest') <- checkYAxisOrChoose (isQuantitative . measurement) rest
  (lc2Y2, _) <- checkYAxis2OrChoose (isQuantitative . measurement) rest'
  pure LineChartSpec2Axis{lc2X, lc2Y, lc2Y2}

heatmap :: forall f m. (Relation f, MonadLogic m, MonadReader (Selections Maybe f) m) => m (HeatmapSpec f)
heatmap = do
  checkMark Rect
  checkArchetype Heatmap
  (hsX, rest) <- asks _WildCards >>= checkXAxisOrChoose (isBarChartDim . measurement)
  (hsY, rest') <- checkYAxisOrChoose (isBarChartDim . measurement) rest
  hsMeasure <- chooseColor (isBarChartDim . measurement) rest' >>= maybe empty pure . fst
  pure HeatmapSpec{hsX, hsY, hsMeasure}

makeChart :: Relation f => Rule f
makeChart =
  fmap Charts.HorizontalBarChart horizontalBarChart
  <|> fmap Charts.VerticalBarChart verticalBarChart
  <|> fmap Charts.Scatterplot scatterplot
  <|> fmap Charts.LineChart linechart
  <|> fmap Charts.LineChart2Axis linechart2
  <|> fmap Charts.Heatmap heatmap
