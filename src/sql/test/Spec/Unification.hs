{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Spec.Unification(
  tests
) where

import           Control.Monad.Except  (runExcept)
import           EasyBI.Sql.Types      (SqlType (..), TyVar (..), apply, mgu)
import qualified Spec.Generators       as Gen
import           Spec.Predicate        (Predicate (..), PredicateValue (..),
                                        forAllValues)
import           Test.Tasty            (TestTree, testGroup)
import           Test.Tasty.HUnit      (Assertion, assertEqual, testCase)
import           Test.Tasty.QuickCheck (Gen, oneof, suchThat, testProperty)

tests :: TestTree
tests =
  testGroup "unification"
    [ testGroup "unit tests"
      [ testCase "mgu1" (checkUnification $ ShouldUnify STBool (STVar 0))
      , testCase "mgu2" (checkUnification $ ShouldNotUnify STBool STNumber)
      , testCase "mgu3" (checkUnification $ ShouldNotUnify (STVar 0) (STArr (STVar 0) (STVar 0)))
      , testCase "mgu4" (checkUnification $ ShouldUnify (STArr STBool STBool) (STArr (STVar 0) (STVar 0)))
      , testCase "mgu5" (checkUnification $ ShouldNotUnify (STArr STBool STNumber) (STArr (STVar 0) (STVar 0)))
      ]
    , testGroup "properties"
      [ testProperty "unification (primitive types)" (forAllValues (Predicate primTypes) (isValid . unify))
      , testProperty "unification (generalised)" (forAllValues (Predicate typeVars) (isValid . unify))
      ]
    ]

checkUnification :: Unify -> Assertion
checkUnification = \case
  ShouldUnify a b ->
    either (fail . (<>) "expectedUnification: " . show) (\subs -> assertEqual ("applying substition: " <> show subs) (apply subs a) (apply subs b)) (runExcept (mgu a b))
  ShouldNotUnify a b ->
    either (const (pure ())) (const (fail "expected unification to fail")) (runExcept (mgu a b))

isValid :: Unify -> Bool
isValid = \case
  ShouldUnify a b -> case runExcept (mgu a b) of
    Left _     -> False
    Right subs -> apply subs a == apply subs b
  ShouldNotUnify a b -> case runExcept (mgu a b) of
    Left{}  -> True
    Right{} -> False

data Unify =
  ShouldUnify (SqlType TyVar) (SqlType TyVar)
  | ShouldNotUnify (SqlType TyVar) (SqlType TyVar)

primTypes :: Gen (PredicateValue (SqlType TyVar, SqlType TyVar))
primTypes =
  let pos = do
        tp <- Gen.sqlTypePrim
        pure (Valid (tp, tp))
      neg = do
        tp1 <- Gen.sqlTypePrim
        tp2 <- Gen.sqlTypePrim `suchThat` (/= tp1)
        pure (Invalid (tp1, tp2))
  in oneof [pos, neg]

typeVars :: Gen (PredicateValue (SqlType TyVar, SqlType TyVar))
typeVars =
  let pos = do
        tp <- Gen.sqlTypePrim
        tp' <- Gen.generalise tp
        pure (Valid (tp, tp'))
      neg = do
        tp <- Gen.sqlTypePrim
        tp' <- Gen.generalise tp
        md <- Gen.modify tp'
        case md of
          Just tp'' -> pure (Invalid (tp, tp''))
          Nothing   -> pure (Valid (tp, tp'))
  in oneof [pos, neg]

unify :: PredicateValue (SqlType TyVar, SqlType TyVar) -> Unify
unify = \case
  Valid (a, b)   -> ShouldUnify a b
  Invalid (a, b) -> ShouldNotUnify a b
