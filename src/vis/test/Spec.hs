{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main
  ( main
  ) where

import Control.Lens          ((&), (.~))
import Control.Monad.Logic   (Logic)
import Control.Monad.Logic   qualified as LogicT
import Control.Monad.Reader  (ReaderT, runReaderT)
import Data.Maybe            (listToMaybe)
import Data.Text             qualified as Text
import EasyBI.Vis.Charts     (Chart (..), HeatmapSpec (..),
                              HorizontalBarChartSpec (..), LineChartSpec (..),
                              LineChartSpec2Axis (..), ScatterplotSpec (..),
                              VerticalBarChartSpec (..))
import EasyBI.Vis.Rules      qualified as Rules
import EasyBI.Vis.Types      (Archetype, Mark (..), Measurement (..),
                              Relation (..), Selections (..), emptySelections,
                              wildCards)
import Test.QuickCheck.Gen   qualified as Gen
import Test.Tasty            (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit      (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck (Arbitrary (..), Property, testProperty)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "scatterplot" scatterplot
  , testCase "2 lines" twoLines
  , testProperty "generate good charts" generateGoodCharts
  ]

twoLines :: Assertion
twoLines = do
  let sel = emptySelections & wildCards .~ [Price, Mileage, PurchaseDate]
      expected = LineChartSpec2Axis{lc2X = PurchaseDate, lc2Y = Price, lc2Y2 = Mileage}

  assertEqual "there should be an encoding with two lines" (Just expected) (runRule Rules.linechart2 sel)

{-| Fig. 4 in the paper
-}
scatterplot :: Assertion
scatterplot = do
  let sel = emptySelections & wildCards .~ [Price, Mileage]
      expected = ScatterplotSpec{spX = Price, spY = Mileage, spColor = Nothing}
  assertEqual "there should be a scatterplot encoding" (Just expected) (runRule Rules.scatterplot sel)

data CarsData = Price | Mileage | PurchaseDate | Model | Make | Decade
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Relation CarsData where
  measurement = \case
    Price        -> Quantitative
    Mileage      -> Quantitative
    PurchaseDate -> TemporalAbs
    Model        -> Nominal
    Make         -> Nominal
    Decade       -> Ordinal

  fieldName = Text.pack . show

runRule :: ReaderT (Selections Maybe f) Logic a -> Selections Maybe f -> Maybe a
runRule rule = listToMaybe . LogicT.observeMany 1 . runReaderT rule

runRuleList :: ReaderT (Selections Maybe f) Logic a -> Selections Maybe f -> [a]
runRuleList rule = LogicT.observeAll . runReaderT rule

instance Arbitrary CarsData where
  arbitrary = Gen.elements (enumFromTo minBound maxBound)

instance Arbitrary Mark where
  arbitrary = Gen.elements (enumFromTo minBound maxBound)

instance Arbitrary Archetype where
  arbitrary = Gen.elements (enumFromTo minBound maxBound)

instance Arbitrary a => Arbitrary (Selections Maybe a) where
  arbitrary =
    Selections
      <$> Gen.listOf arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> pure Nothing
      <*> pure Nothing

  shrink = QC.genericShrink

-- | Check that we don't generate any nonsensical charts
generateGoodCharts :: Property
generateGoodCharts = QC.forAllShrink @(Selections Maybe CarsData) arbitrary shrink $ \selections -> all isGoodChart (runRuleList Rules.makeChart selections)

-- | Check if the dimensions of the chart make any sense.
isGoodChart :: Relation f => Chart f -> Bool
isGoodChart = \case
  VerticalBarChart VerticalBarChartSpec{vbcColor}     -> colorOK vbcColor
  HorizontalBarChart HorizontalBarChartSpec{hbcColor} -> colorOK hbcColor
  Scatterplot ScatterplotSpec{spColor}                -> colorOK spColor
  LineChart LineChartSpec{lcX, lcY, lcColor} ->
    colorOK lcColor
    && Rules.isTemporal (measurement lcX)
    && Rules.isQuantitative (measurement lcY)
  LineChart2Axis LineChartSpec2Axis{lc2X, lc2Y, lc2Y2} ->
    Rules.isTemporal (measurement lc2X)
    && Rules.isQuantitative (measurement lc2Y)
    && Rules.isQuantitative (measurement lc2Y2)
  Heatmap HeatmapSpec{} -> True

colorOK :: Relation f => Maybe f -> Bool
colorOK = maybe True (k . measurement) where
  k m = not (Rules.isTemporal m || Rules.isQuantitative m)
