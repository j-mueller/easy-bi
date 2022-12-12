{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main(main) where

import           Control.Monad                  (when)
import           Data.Bifunctor                 (Bifunctor (..))
import           Data.Foldable                  (traverse_)
import           EasyBI.Sql.Types               (AnnotateErr, InferError (..),
                                                 SqlType (..), SqlVar (..),
                                                 TyVar (..),
                                                 UnificationError (..),
                                                 defaultTypeEnv, runInferType,
                                                 typeConstraints)
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
      [ testCaseSteps "+ (1)" (checkInference $ ShouldInferSuccess [(":x", STNumber)] "1 + :x")
      , testCaseSteps "+ (2)" (checkInference $ ShouldInferSuccess [(":x", STNumber), (":x2", STNumber)] ":x2 + :x")
      , testCaseSteps "+ (3)" (checkInference $ ShouldInferFail (IUnificationError (UnificationError STBool STNumber)) ":x2 + true")
      , testCaseSteps "+, *" (checkInference $ ShouldInferSuccess [(":x", STNumber)] ":x + (1 + 3) * 4")
      , testCaseSteps "IN" (checkInference $ ShouldInferSuccess [(":x", STText), (":y", STText)] ":x IN ('a', 'b', c.y, :y)")
      , testCaseSteps "OR, >" (checkInference $ ShouldInferSuccess [(":x", STText), (":y", STText), (":z", STNumber)] ":x IN ('a', 'b', c.y, :y) OR (:z > 4)")
      , testCaseSteps "=" (checkInference $ ShouldInferSuccess [(":x", STText)] ":x = 'München'")
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

checkInference :: Infer -> (String -> IO ()) -> Assertion
checkInference i step = case i of
  ShouldInferSuccess expectedTypes expression -> do
    expr' <- parseExpr expression
    case runInferType defaultTypeEnv expr' of
      Left err -> do
        step (show expr')
        fail ("checkInference: Expected inference to succeed, but it failed with " <> show err)
      Right (substition, exprType, assumptions) -> do
        let expected' = fmap (first (\nm -> AHostParameter nm Nothing)) expectedTypes
        flip traverse_ expected' $ \typeAssignment -> do
          let result = typeAssignment `elem` assumptions
          when (not result) $ do
            step (show expr')
            step (show substition)
            step (show exprType)
            step (show assumptions)
          assertBool ("expected " <> show (fst typeAssignment) <> " to have type " <> show (snd typeAssignment)) result
  ShouldInferFail err expression -> do
    expr' <- parseExpr expression
    case runInferType defaultTypeEnv expr' of
      Left actual -> assertEqual "checkInference: expected error" err actual
      Right result -> fail ("Expected type inference to fail, but it succeeded with " <> show result)


data Infer =
  {-| Inference should succeed, assigning the expected types to the variables
  -}
  ShouldInferSuccess [(String, SqlType TyVar)] String

  {-| Inference should fail
  -}
  | ShouldInferFail InferError String
