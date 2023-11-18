{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Main
  ( main
  ) where

import Control.Monad               (unless)
import Data.Foldable               (traverse_)
import Data.Map                    qualified as Map
import EasyBI.Server.Eval          (DbBackend (..), evalQuery,
                                    withDbConnectionPool)
import EasyBI.Server.Visualisation (AMeasurement (..), FieldInMode (..),
                                    Filter (..), MeasurementAndField (..),
                                    SortOrder (..), visualisations)
import EasyBI.Sql.Catalog          (Catalog (..), TypedQueryExpr (..))
import EasyBI.Sql.Construct        qualified as C
import EasyBI.Sql.Effects.Types    (Tp (..), TyScheme (..), TyVar)
import EasyBI.Test.Utils           (SampleDB (..), parseQuery, sampleCatalog,
                                    withSampleDb, withTempDir)
import EasyBI.Vis.Types            (Selections (..))
import Test.Tasty                  (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit            (Assertion, assertBool, assertEqual,
                                    testCase, testCaseSteps)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "server"
  [ testCaseSteps "visualisations" checkVisualisations
  , testCase "parse json_object" checkSqliteJSON
  , testGroup "sample datasets"
    [ testCase "sales" (checkSampleCatalog Sales)
    , testCase "outages" (checkSampleCatalog Outages)
    ]
  ]

checkVisualisations :: (String -> IO ()) -> Assertion
checkVisualisations step = do
  let category = FieldInMode "category" Nothing MeasurementAndField{mFieldType=ANominalMeasurement, mFilter=NoFilter} Ascending
      price    = FieldInMode "price" Nothing MeasurementAndField{mFieldType=AQuantitativeMeasurement, mFilter=NoFilter} Ascending
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
    withSampleDb tmp Sales $ \(SqliteBackend -> db) -> do
      withDbConnectionPool db $ \pool -> do
        qry <- parseQuery "select ORDERNUMBER as c from sales limit 10"
        results <- evalQuery pool qry
        assertEqual "there should be ten results" 10 (length results)

checkSampleCatalog :: SampleDB -> Assertion
checkSampleCatalog sampleDB = do
  withTempDir "sample-db" $ \tmp -> do
    withSampleDb tmp sampleDB $ \(SqliteBackend -> db) -> do
      withDbConnectionPool db $ \pool -> do
        Catalog{_views} <- sampleCatalog sampleDB
        case Map.toList _views of
          [(_, TypedQueryExpr{teQuery, teType})] -> do
            checkType sampleDB teType
            results <- evalQuery pool teQuery
            assertBool "there should be at least one result" (length results > 0)
          _ -> assertEqual "Expect 1 view" 1 (length _views)

checkType :: SampleDB -> TyScheme TyVar (Tp TyVar) -> IO ()
checkType db (TyScheme _ tyScheme) =
  let expected = case db of
        Sales   ->
          C.row 44
            [ ("count", C.number)
            , ("country", C.text)
            , ("customername", C.text)
            , ("dealsize", C.text)
            , ("productcode", C.text)
            , ("productline", C.text)
            , ("status", C.text)
            , ("territory", C.text)
            , ("totalPriceEach", C.number)
            , ("totalSales", C.number)
            ]
        Outages ->
          C.row 39
            [ ("city", C.text)
            , ("count", C.number)
            , ("duration_avg", C.number)
            , ("duration_sum", C.number)
            , ("operator", C.text)
            , ("outage_date", C.datetime)
            , ("outage_day_of_month", C.temporal)
            , ("outage_hour", C.temporal)
            , ("outage_month", C.temporal)
            ]
  in assertEqual "schema" expected tyScheme
