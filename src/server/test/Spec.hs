{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Data.Foldable                 (traverse_)
import EasyBI.Server.Visualisation   (visualisations)
import EasyBI.Sql.Effects.Types      (SqlType (..), Tp (..), TyScheme (..),
                                      mkRow)
import Language.SQL.SimpleSQL.Syntax (Name (..))
import Test.Tasty                    (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit              (Assertion, assertBool, testCaseSteps)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "server"
  [ testCaseSteps "visualisations" checkVisualisations

  ]

checkVisualisations :: (String -> IO ()) -> Assertion
checkVisualisations step = do
  let vis = visualisations (TyScheme [] $ TpRow $ mkRow 0 [(n "revenue", number), (n "country", text), (n "year", int)])
  traverse_ (step . show) vis
  assertBool "there should be some visualisations" $ not $ null vis

number :: Tp v
number = TpSql STNumber

text :: Tp v
text = TpSql STText

int :: Tp v
int = TpSql STInt

n :: String -> Name
n = Name Nothing
