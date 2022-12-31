{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
module Spec.Unification
  ( tests
  ) where

import Control.Monad.Except     (runExcept)
import EasyBI.Sql.Effects.Fresh (evalFreshT)
import EasyBI.Sql.Effects.Types (Tp (..))
import EasyBI.Sql.Types         (SqlType (..), TyVar (..), apply, mgu)
import Spec.Generators          qualified as Gen
import Spec.Predicate           (Predicate (..), PredicateValue (..),
                                 forAllValues)
import Test.Tasty               (TestTree, testGroup)
import Test.Tasty.HUnit         (Assertion, assertEqual, testCase)
import Test.Tasty.QuickCheck    (Gen, oneof, testProperty)

tests :: TestTree
tests =
  testGroup "unification"
    [ testGroup "unit tests"
      [ testCase "mgu1" (checkUnification $ ShouldUnify (TpSql STBool) (TpVar 0))
      , testCase "mgu2" (checkUnification $ ShouldNotUnify (TpSql STBool) (TpSql STNumber))
      , testCase "mgu3" (checkUnification $ ShouldNotUnify (TpVar 0) (TpArr (TpVar 0) (TpVar 0)))
      , testCase "mgu4" (checkUnification $ ShouldUnify (TpArr (TpSql STBool) (TpSql STBool)) (TpArr (TpVar 0) (TpVar 0)))
      , testCase "mgu5" (checkUnification $ ShouldNotUnify (TpArr (TpSql STBool) (TpSql STNumber)) (TpArr (TpVar 0) (TpVar 0)))
      ]
    , testGroup "properties"
      [ testProperty "unification (primitive types)" (forAllValues (Predicate primTypes) (isValid . unify))
      , testProperty "unification (generalised)" (forAllValues (Predicate typeVars) (isValid . unify))
      ]
    ]

checkUnification :: Unify -> Assertion
checkUnification = \case
  ShouldUnify a b ->
    either (fail . (<>) "expectedUnification: " . show) (\subs -> assertEqual ("applying substition: " <> show subs) (apply subs a) (apply subs b)) (runExcept (evalFreshT $ mgu a b))
  ShouldNotUnify a b ->
    either (const (pure ())) (const (fail "expected unification to fail")) (runExcept (evalFreshT $ mgu a b))

isValid :: Unify -> Bool
isValid = \case
  ShouldUnify a b -> case runExcept $ evalFreshT $ mgu a b of
    Left _     -> False
    Right subs -> apply subs a == apply subs b
  ShouldNotUnify a b -> case runExcept (evalFreshT $ mgu a b) of
    Left{}  -> True
    Right{} -> False

data Unify =
  ShouldUnify (Tp TyVar) (Tp TyVar)
  | ShouldNotUnify (Tp TyVar) (Tp TyVar)

primTypes :: Gen (PredicateValue (Tp TyVar, Tp TyVar))
primTypes =
  let pos = do
        tp <- TpSql <$> Gen.sqlTypePrim
        pure (Valid (tp, tp))
      neg = do
        tp1 <- TpSql <$> Gen.sqlTypePrim
        md <- Gen.modify tp1
        case md of
          Just tp2 -> pure (Invalid (tp1, tp2))
          Nothing  -> pure (Valid (tp1, tp1))
  in oneof [pos, neg]

typeVars :: Gen (PredicateValue (Tp TyVar, Tp TyVar))
typeVars =
  let pos = do
        tp <- TpSql <$> Gen.sqlTypePrim
        tp' <- Gen.generalise tp
        pure (Valid (tp, tp'))
      neg = do
        tp <- TpSql <$> Gen.sqlTypePrim
        tp' <- Gen.generalise tp
        md <- Gen.modify tp'
        case md of
          Just tp'' | tp'' /= tp -> pure (Invalid (tp, tp''))
          _                      -> pure (Valid (tp, tp'))
  in oneof [pos, neg]

unify :: PredicateValue (Tp TyVar, Tp TyVar) -> Unify
unify = \case
  Valid (a, b)   -> ShouldUnify a b
  Invalid (a, b) -> ShouldNotUnify a b
