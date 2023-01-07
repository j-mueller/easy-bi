{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Main
  ( main
  ) where

import Data.Foldable                 (traverse_)
import EasyBI.Sql.Effects.Types      (Tp (..), mkRow)

import Data.Map.Strict               qualified as Map
import Data.Proxy                    (Proxy (..))
import EasyBI.Sql.Class              (SqlFragment (..), ansi2011, runInferType,
                                      typeConstraints)
import EasyBI.Sql.Types              (AnnotateErr, InferError (..),
                                      SqlType (..), SqlVar (..), TyVar (..),
                                      UnificationError (..), defaultTypeEnv,
                                      getFailure)
import EasyBI.Sql.Utils              qualified as Utils
import Language.SQL.SimpleSQL.Syntax (Name (..), QueryExpr, ScalarExpr)
import Spec.Unification              qualified as Unification
import Test.Tasty                    (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit              (Assertion, assertBool, assertEqual,
                                      testCase, testCaseSteps)

main :: IO ()
main = defaultMain tests

scalar :: Proxy ScalarExpr
scalar = Proxy

query :: Proxy QueryExpr
query = Proxy

tests :: TestTree
tests = testGroup "type inference"
  [ testGroup "annotations"
      [ testCase "annotate simple expr" simpleExpr
      , testCase "annotate host param" hostParam
      ]
  , Unification.tests
  , testGroup "inference"
      [ testGroup "scalar expressions"
        [ testCaseSteps "+ (1)" (inferSuccess scalar number [(p ":x", number)] "1 + :x")
        , testCaseSteps "+ (2)" (inferSuccess scalar number [(p ":x", number), (p ":x2", number)] ":x2 + :x")
        , testCaseSteps "+ (3)" (inferFail scalar (IUnificationError [] (UnificationFailed (TpSql STBool) (number))) ":x2 + true")
        , testCaseSteps "+, *" (inferSuccess scalar number [(p ":x", number)] ":x + (1 + 3) * 4")
        , testCaseSteps "IN" (inferSuccess scalar boolean [(p ":x", text), (p ":y", text)] ":x IN ('a', 'b', c.y, :y)")
        , testCaseSteps "OR, >" (inferSuccess scalar boolean [(p ":x", text), (p ":y", text), (p ":z", number)] ":x IN ('a', 'b', c.y, :y) OR (:z > 4)")
        , testCaseSteps "=" (inferSuccess scalar boolean [(p ":x", text)] ":x = 'München'")
        , testCaseSteps "row (1)" (inferSuccess scalar number [(p ":x", number), dot 2 "c" [("y", number)]] ":x + c.y")
        , testCaseSteps "row (2)" (inferSuccess scalar boolean [(p ":x", number), dot 2 "c" [("y", number)]] "(:x = c.y) AND (:x + 5 > 10)")
        , testCaseSteps "row (3)" (inferSuccess scalar number [dot 12 "c" [("y", number), ("z", number)]] "c.y + c.z")
        , testCaseSteps "row (4)" (inferSuccess scalar boolean [dot 14 "c" [("y", number), ("t", text)]] "(c.y > 6) OR (c.t = 'a')")
        , testCaseSteps "row (5)" (inferSuccess scalar boolean [(p ":y", number), (p ":t", text)] "(:y > 6) OR (:t = 'a')")
        , testCaseSteps "polymorphic equals" (inferSuccess scalar boolean [(p ":y", number), (p ":t", text)] "(:t = 'a') OR (:y = 2)")
        ]
      , testGroup "select queries"
        [ testCaseSteps "SELECT (1)" (inferSuccess query (row 4 [("b", TpVar (TyVar 0))]) [] "select a.b from a")
        , testCaseSteps "SELECT (2)" (inferSuccess query (row 0 [("b", number)]) [] "select 10 as b from a")

        -- infer the type of 'd' from the operator
        , testCaseSteps "SELECT (3)" (inferSuccess query (row 3 [("b", number), ("d", number)]) [] "select 10 as b, c + c as d from a")
        ]
      ]
  ]

simpleExpr :: Assertion
simpleExpr = do
  (tp, (assumptions, constraints)) <- fmap typeConstraints (parse' scalar "1+1") >>= annotateErr
  assertEqual "operator assumption" [(AnOperator [Name Nothing "+"],TpArr (TpSql STNumber) (TpArr (TpSql STNumber) (TpVar 0)))] assumptions
  assertEqual "operator constraints" [] constraints
  assertEqual "operator type" (TpVar 0) tp

hostParam :: Assertion
hostParam = do
  (tp, (assumptions, constraints)) <- fmap typeConstraints (parse' scalar "1 + :x") >>= annotateErr
  assertEqual "operator assumption" [(AHostParameter ":x" Nothing, TpVar 0),(AnOperator [Name Nothing "+"], TpArr (TpSql STNumber) (TpArr (TpVar 0) (TpVar 1)))] assumptions
  assertEqual "operator constraints" [] constraints
  assertEqual "operator type" (TpVar 1) tp

parse' :: SqlFragment a => Proxy a -> String -> IO a
parse' _p = either (fail . show) pure . parse ansi2011 "" Nothing

annotateErr :: Either AnnotateErr a -> IO a
annotateErr = either (fail . show) pure

checkInference :: Infer -> (String -> IO ()) -> Assertion
checkInference i step = case i of
  ShouldInferSuccess p_ expectedType expectedTypes expression -> do
    expr' <- parse' p_ expression
    case runInferType defaultTypeEnv expr' of
      Left err -> do
        step expression
        step (show expr')
        case typeConstraints expr' of
          Right (_, (assumptions, constraints)) -> do
            step ("assumptions: " <> Utils.renderString assumptions)
            step ("constraints: " <> Utils.renderString constraints)
            step ("error:       " <> Utils.renderString err)
            assertBool ("checkInference: Expected inference to succeed, but it failed") False
          Left annotateErr_ -> do
            step ("annotateErr: " <> show annotateErr_)
            assertBool ("checkInference: Expected inference to succeed, but it failed in the annotation phase") False
      Right (substitution, exprType, assignments) -> do
        flip traverse_ expectedTypes $ \(var_, expType) -> do
          case Map.lookup var_ assignments of
            Nothing -> do
                case typeConstraints expr' of
                  Right (_, (assumptions2, constraints)) -> do
                    step ("expr':         " <> show expr')
                    step ("exprType:      " <> Utils.renderString exprType)
                    step ("assignments:   " <> Utils.renderString (Map.toList assignments))
                    step ("assumptions2:  " <> Utils.renderString assumptions2)
                    step ("constraints:   " <> Utils.renderString constraints)
                    step ("substitution:  " <> Utils.renderString substitution)
                    assertBool ("checkInference: Failed to find type for " <> show var_) False
                  Left annotateErr_ -> do
                    step ("annotateErr: " <> show annotateErr_)
                    assertBool ("checkInference: Expected inference to succeed, but it failed in the 2nd annotation phase") False
            Just actualType ->
              assertEqual (show var_) expType actualType
        assertEqual "Fragment type"  expectedType exprType
  ShouldInferFail p_ err expression -> do
    expr' <- parse' p_ expression
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
dot v a values = (AnIdentifier [Name Nothing a], row v values)

row :: TyVar -> [(String, Tp TyVar)] -> Tp TyVar
row v values =
  let f (n, t) = (Name Nothing n, t)
  in TpRow $ mkRow v (f <$> values)

data Infer =
  {-| Inference should succeed, assigning the expected types to the variables
  -}
  forall a. (Show a, SqlFragment a) => ShouldInferSuccess (Proxy a) (Tp TyVar) [(SqlVar, Tp TyVar)] String

  {-| Inference should fail
  -}
  | forall a. (Show a, SqlFragment a) => ShouldInferFail (Proxy a) InferError String

inferSuccess :: (Show a, SqlFragment a) => Proxy a -> (Tp TyVar) -> [(SqlVar, Tp TyVar)] -> String -> (String -> IO ()) -> Assertion
inferSuccess p_ t a = checkInference . ShouldInferSuccess p_ t a

inferFail :: (Show a, SqlFragment a) => Proxy a -> InferError -> String -> (String -> IO ()) -> Assertion
inferFail p_ a = checkInference . ShouldInferFail p_ a

number :: Tp v
number = TpSql STNumber

text :: Tp v
text = TpSql STText

boolean :: Tp v
boolean = TpSql STBool
