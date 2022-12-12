{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
module Spec.Predicate(
  Predicate(..),
  PredicateValue(..),
  predicateValues,
  forAllValues,
  getA,
  and_
  ) where

import           Control.Applicative   (liftA2)
import           Test.Tasty.QuickCheck (Gen, Property, Testable, forAll, label)

newtype Predicate a = Predicate{ unPredicate :: Gen (PredicateValue a) }

{-| A @Predicate@ that is true for all values
-}
alwaysHolds :: a -> Predicate a
alwaysHolds a = Predicate (pure (Valid a))

predicateValues :: Predicate a -> Gen (PredicateValue a)
predicateValues = unPredicate

{-| Conjunction of two predicates
-}
and_ :: forall a b. Predicate a -> Predicate b -> Predicate (a, b)
and_ = liftA2 (,)

data PredicateValue a =
  Valid a
  | Invalid a
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

getA :: PredicateValue a -> a
getA = \case
  Valid a   -> a
  Invalid a -> a

instance Applicative PredicateValue where
  pure = Valid
  liftA2 f l r = case (l, r) of
    (Valid x, Valid y) -> Valid (f x y)
    (x, y)             -> Invalid (f (getA x) (getA y))

instance Monad PredicateValue where
  x >>= f = case x of
    Valid x'   -> f x'
    Invalid x' -> Invalid $ getA $ f x'

instance Functor Predicate where
  fmap f = Predicate . fmap (fmap f) . unPredicate

instance Applicative Predicate where
  pure = alwaysHolds
  liftA2 f (Predicate l) (Predicate r) =
    Predicate (liftA2 f <$> l <*> r)

forAllValues :: (Show a, Testable prop) => Predicate a -> (PredicateValue a -> prop) -> Property
forAllValues p t =
  let lbl = \case
              Valid{} -> "valid"
              Invalid{} -> "invalid"
  in forAll (predicateValues p) (\pv -> label (lbl pv) (t pv))
