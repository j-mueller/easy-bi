{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main
  ( main
  ) where

import Control.Lens     ((&), (.~))
import Data.Text        qualified as Text
import EasyBI.Vis.Rules (makeChart)
import EasyBI.Vis.Types (Archetype (..), Mark (..), Measurement (..),
                         Relation (..), archetype, emptyEncoding,
                         emptySelections, fieldPositionChannel, markChannel,
                         positionX, positionY, runRule, wildCards)
import Test.Tasty       (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "scatterplot" scatterplot

  ]

{-| Fig. 4 in the paper
-}
scatterplot :: Assertion
scatterplot = do
  let sel = emptySelections & wildCards .~ [Price, Mileage]
      encodings = runRule 1 makeChart sel
      expected = emptyEncoding
                  & positionX .~ Just (fieldPositionChannel Mileage)
                  & positionY .~ Just (fieldPositionChannel Price)
                  & markChannel .~ Just Point
                  & archetype .~ Just Scatterplot
  case encodings of
    (x:_) -> assertEqual "Scatterplot encoding" expected x
    _     -> error "Empty encodings"

data CarsData = Price | Mileage
  deriving (Eq, Ord, Show)

instance Relation CarsData where
  measurement = \case
    Price   -> Quantitative
    Mileage -> Quantitative

  fieldName = Text.pack . show
