{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import           EasyBI.Sql.Types               (AnnotateErr, SqlType (..),
                                                 SqlVar (..),
                                                 TyVar (..), typeConstraints)
import qualified Language.SQL.SimpleSQL.Dialect as Dialect
import qualified Language.SQL.SimpleSQL.Parse   as Parse
import           Language.SQL.SimpleSQL.Syntax  (Name (..), ScalarExpr)
import           Test.Tasty                     (TestTree, defaultMain,
                                                 testGroup)
import           Test.Tasty.HUnit               (Assertion, assertEqual,
                                                 testCase)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "type inference"
  [ testGroup "annotations"
      [ testCase "annotate simple expr" simpleExpr
      , testCase "annotate host param" hostParam
      ]
  ]

simpleExpr :: Assertion
simpleExpr = do
  (tp, (assumptions, constraints)) <- fmap typeConstraints (parseExpr "1+1") >>= annotateErr
  assertEqual "operator assumption" [(AnOperator [Name Nothing "+"],STArr STNumber (STArr STNumber (STVar (TyVar 0))))] assumptions
  assertEqual "operator constraints" [] constraints
  assertEqual "operator type" (STVar (TyVar 0)) tp

hostParam :: Assertion
hostParam = do
  (tp, (assumptions, constraints)) <- fmap typeConstraints (parseExpr "1 + :x") >>= annotateErr
  assertEqual "operator assumption" [(AHostParameter ":x" Nothing,STVar (TyVar 0)),(AnOperator [Name Nothing "+"],STArr STNumber (STArr (STVar (TyVar 0)) (STVar (TyVar 1))))] assumptions
  assertEqual "operator constraints" [] constraints
  assertEqual "operator type" (STVar (TyVar 1)) tp

parseExpr :: String -> IO ScalarExpr
parseExpr = either (fail . show) pure . Parse.parseScalarExpr Dialect.ansi2011 "" Nothing

annotateErr :: Either AnnotateErr a -> IO a
annotateErr = either (fail . show) pure
