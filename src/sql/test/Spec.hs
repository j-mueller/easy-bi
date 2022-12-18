{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import           Data.Foldable                  (traverse_)
import           EasyBI.Sql.Effects.Types       (Tp (..), mkRow)

import qualified Data.Map.Strict                as Map
import           EasyBI.Sql.Types               (AnnotateErr, InferError (..),
                                                 SqlType (..), SqlVar (..),
                                                 TyVar (..),
                                                 UnificationError (..),
                                                 defaultTypeEnv, getFailure,
                                                 runInferType, typeConstraints)
import qualified EasyBI.Sql.Utils               as Utils
import qualified Language.SQL.SimpleSQL.Dialect as Dialect
import qualified Language.SQL.SimpleSQL.Parse   as Parse
import           Language.SQL.SimpleSQL.Syntax  (Name (..), ScalarExpr)
import qualified Spec.Unification               as Unification
import           Test.Tasty                     (TestTree, defaultMain,
                                                 testGroup)
import           Test.Tasty.HUnit               (Assertion, assertBool,
                                                 assertEqual, testCase,
                                                 testCaseSteps)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "type inference"
  [ testGroup "annotations"
      [ testCase "annotate simple expr" simpleExpr
      , testCase "annotate host param" hostParam
      ]
  , Unification.tests
  , testGroup "inference"
      [ testCaseSteps "+ (1)" (checkInference $ ShouldInferSuccess [(p ":x", TpSql STNumber)] "1 + :x")
      , testCaseSteps "+ (2)" (checkInference $ ShouldInferSuccess [(p ":x", TpSql STNumber), (p ":x2", TpSql STNumber)] ":x2 + :x")
      , testCaseSteps "+ (3)" (checkInference $ ShouldInferFail (IUnificationError [] (UnificationFailed (TpSql STNumber) (TpSql STBool))) ":x2 + true")
      , testCaseSteps "+, *" (checkInference $ ShouldInferSuccess [(p ":x", TpSql STNumber)] ":x + (1 + 3) * 4")
      , testCaseSteps "IN" (checkInference $ ShouldInferSuccess [(p ":x", TpSql STText), (p ":y", TpSql STText)] ":x IN ('a', 'b', c.y, :y)")
      , testCaseSteps "OR, >" (checkInference $ ShouldInferSuccess [(p ":x", TpSql STText), (p ":y", TpSql STText), (p ":z", TpSql STNumber)] ":x IN ('a', 'b', c.y, :y) OR (:z > 4)")
      , testCaseSteps "=" (checkInference $ ShouldInferSuccess [(p ":x", TpSql STText)] ":x = 'München'")
      , testCaseSteps "row (1)" (checkInference $ ShouldInferSuccess [(p ":x", TpSql STNumber), dot 2 "c" [("y", TpSql STNumber)]] ":x + c.y")
      , testCaseSteps "row (2)" (checkInference $ ShouldInferSuccess [(p ":x", TpSql STNumber), dot 2 "c" [("y", TpSql STNumber)]] "(:x = c.y) AND (:x + 5 > 10)")
      , testCaseSteps "row (3)" (checkInference $ ShouldInferSuccess [dot 13 "c" [("y", TpSql STNumber), ("z", TpSql STNumber)]] "c.y + c.z")
      , testCaseSteps "row (4)" (checkInference $ ShouldInferSuccess [dot 17 "c" [("y", TpSql STNumber), ("t", TpSql STText)]] "(c.y > 6) OR (c.t = 'a')")
      , testCaseSteps "row (5)" (checkInference $ ShouldInferSuccess [(p ":y", TpSql STNumber), (p ":t", TpSql STText)] "(:y > 6) OR (:t = 'a')")
      ]
  ]

simpleExpr :: Assertion
simpleExpr = do
  (tp, (assumptions, constraints)) <- fmap typeConstraints (parseExpr "1+1") >>= annotateErr
  assertEqual "operator assumption" [(AnOperator [Name Nothing "+"],TpArr (TpSql STNumber) (TpArr (TpSql STNumber) (TpVar 0)))] assumptions
  assertEqual "operator constraints" [] constraints
  assertEqual "operator type" (TpVar 0) tp

hostParam :: Assertion
hostParam = do
  (tp, (assumptions, constraints)) <- fmap typeConstraints (parseExpr "1 + :x") >>= annotateErr
  assertEqual "operator assumption" [(AHostParameter ":x" Nothing, TpVar 0),(AnOperator [Name Nothing "+"], TpArr (TpSql STNumber) (TpArr (TpVar 0) (TpVar 1)))] assumptions
  assertEqual "operator constraints" [] constraints
  assertEqual "operator type" (TpVar 1) tp

parseExpr :: String -> IO ScalarExpr
parseExpr = either (fail . show) pure . Parse.parseScalarExpr Dialect.ansi2011 "" Nothing

annotateErr :: Either AnnotateErr a -> IO a
annotateErr = either (fail . show) pure

checkInference :: Infer -> (String -> IO ()) -> Assertion
checkInference i step = case i of
  ShouldInferSuccess expectedTypes expression -> do
    expr' <- parseExpr expression
    case runInferType defaultTypeEnv expr' of
      Left err -> do
        step expression
        step (show expr')
        let Right (_, (assumptions, constraints)) = typeConstraints expr'
        step (Utils.renderString assumptions)
        step (Utils.renderString constraints)
        step (Utils.renderString err)
        assertBool ("checkInference: Expected inference to succeed, but it failed") False
      Right (substition, exprType, assignments) -> do
        flip traverse_ expectedTypes $ \(var_, expType) -> do
          case Map.lookup var_ assignments of
            Nothing -> do
                let Right (_, (assumptions2, constraints)) = typeConstraints expr'
                step ("expr':         " <> show expr')
                step ("exprType:      " <> Utils.renderString exprType)
                step ("assignments:   " <> Utils.renderString (Map.toList assignments))
                step ("assumptions2:  " <> Utils.renderString assumptions2)
                step ("constraints:   " <> Utils.renderString constraints)
                step ("substitution:  " <> Utils.renderString substition)
            Just actualType -> 
              assertEqual (show var_) expType actualType
  ShouldInferFail err expression -> do
    expr' <- parseExpr expression
    case runInferType defaultTypeEnv expr' of
      Left (IUnificationError x a) -> case err of
        IUnificationError _ b ->
          assertEqual "checkInference: expected error" (getFailure a) (getFailure b)
        _ -> assertEqual "checkInference: expected error" (IUnificationError x a) err
      Left a -> assertEqual "checkInference: expected error" a err
      Right result -> fail ("Expected type inference to fail, but it succeeded with " <> show result)

p :: String -> SqlVar
p n = AHostParameter n Nothing

dot :: TyVar -> String -> [(String, Tp TyVar)] -> (SqlVar, Tp TyVar)
dot v a values =
  let f (n, t) = (Name Nothing n, t)
  in (AnIdentifier [Name Nothing a], TpRow $ mkRow v (f <$> values))

data Infer =
  {-| Inference should succeed, assigning the expected types to the variables
  -}
  ShouldInferSuccess [(SqlVar, Tp TyVar)] String

  {-| Inference should fail
  -}
  | ShouldInferFail InferError String
