{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Main
  ( main
  ) where

import Control.Monad               (unless)
import Data.Foldable               (traverse_)
import EasyBI.Server.Eval          (DbBackend (..), evalQuery,
                                    withDbConnectionPool)
import EasyBI.Server.Visualisation (Field (..), SortOrder (..), visualisations)
import EasyBI.Sql.Catalog          (Catalog (..), TypedQueryExpr (..))
import EasyBI.Test.Utils           (parseQuery, sampleCatalog, withSampleDb,
                                    withTempDir)
import EasyBI.Vis.Types            (Measurement (..), Selections (..))
import Test.Tasty                  (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit            (Assertion, assertBool, assertEqual,
                                    testCase, testCaseSteps)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "server"
  [ testCaseSteps "visualisations" checkVisualisations
  , testCase "parse json_object" checkSqliteJSON
  , testCase "run queries from sample catalog" checkSampleCatalog
  ]

checkVisualisations :: (String -> IO ()) -> Assertion
checkVisualisations step = do
  let category = Field "category" Nominal Ascending
      price    = Field "price"    Quantitative Ascending
      sel =
        Selections
          { _WildCards = [category, price]
          , _XAxis     = []
          , _YAxis     = []
          , _Color     = []
          , _selectedMark = []
          , _selectedArchetype = []
          }
      vis  = visualisations () sel
      good = not (null vis)
  unless good (traverse_ (step . show) vis)
  assertBool "there should be some visualisations" good

checkSqliteJSON :: Assertion
checkSqliteJSON = do
  withTempDir "sample-db" $ \tmp -> do
    withSampleDb tmp $ \(SqliteBackend -> db) -> do
      withDbConnectionPool db $ \pool -> do
        qry <- parseQuery "select ORDERNUMBER as c from sales limit 10"
        results <- evalQuery pool qry
        assertEqual "there should be ten results" 10 (length results)

checkSampleCatalog :: Assertion
checkSampleCatalog = do
  withTempDir "sample-db" $ \tmp -> do
    withSampleDb tmp $ \(SqliteBackend -> db) -> do
      withDbConnectionPool db $ \pool -> do
        Catalog{_views} <- sampleCatalog
        flip traverse_ _views $ \TypedQueryExpr{teQuery} -> do
          results <- evalQuery pool teQuery
          assertBool "there should be at least one result" (length results > 0)
