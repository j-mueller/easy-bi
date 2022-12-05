{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import           Control.Lens     ((&), (.~))
import qualified Data.Text        as Text
import           EasyBI.Vis.Rules (makeChart)
import           EasyBI.Vis.Types (Mark (..), Measurement (..), OutOf (..),
                                   Relation (..), emptyEncoding,
                                   emptySelections, field, mark, markChannel,
                                   positionX, positionY, runRule, wildCards,
                                   wildCardsUsed)
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (Assertion, assertEqual, testCase)

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
      encodings = runRule makeChart sel
      expected = emptyEncoding
                  & positionX . field .~ Just Mileage
                  & positionY . field .~ Just Price
                  & markChannel . mark .~ Just Point
                  & wildCardsUsed .~ Just (2 `OutOf` 2)
  case encodings of
    (x:_) -> assertEqual "Scatterplot encoding" expected x
    _     -> error "Empty encodings"

data CarsData = Price | Mileage
  deriving (Eq, Ord, Show)

instance Relation CarsData where
  measurement = \case
    Price   -> Quantitative
    Mileage -> Quantitative

  dependsOn _ _ = False

  fieldName = Text.pack . show
