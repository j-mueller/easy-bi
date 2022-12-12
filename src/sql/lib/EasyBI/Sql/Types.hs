{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Typing SQL statements. Based on "Generalizing Hindley-Milner Type Inference Algorithms" by B. Heeren, J. Hage and D. Swierstra (technical report)
-}
module EasyBI.Sql.Types(
  TyVar(..),
  SqlType(..),
  SqlVar(..),
  TyConstraint(..),
  AnnotateErr(..),
  UnificationError(..),
  typeConstraints,

  -- * Type enviroments
  TypeEnv(..),
  defaultTypeEnv,

  -- * Substitutions and unifiers
  Substitution,
  apply,
  mgu,
  comp,
  singleton,
  inferType,
  InferError(..),
  runInferType
) where

import           Control.Monad                 (void)
import           Control.Monad.Except          (ExceptT, MonadError (..),
                                                runExceptT)
import           Data.Bifunctor                (Bifunctor (..))
import           Data.Functor.Foldable         (cataA)
import           Data.Functor.Identity         (Identity (..))
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (mapMaybe)
import           EasyBI.Sql.BuiltinTypes       (defaultTypeEnv)
import           EasyBI.Sql.Effects.Annotate   (AnnotateT, MonadAnnotate (..),
                                                runAnnotateT)
import           EasyBI.Sql.Effects.Fresh      (FreshT, MonadFresh (..),
                                                evalFreshT, instantiate)
import           EasyBI.Sql.Effects.Types      (Assumption, Constraint,
                                                SqlType (..), SqlVar (..),
                                                Substitution, TyConstraint (..),
                                                TyVar (..), TypeEnv (..), apply,
                                                comp, freeVars, singleton)
import           EasyBI.Sql.Syntax             (InPredValueF (..),
                                                ScalarExprF (..))
import           Language.SQL.SimpleSQL.Syntax (Name (..), ScalarExpr)

solve :: (MonadError (UnificationError TyVar) m, MonadFresh m) => [TyConstraint TyVar (SqlType TyVar)] -> m (Substitution TyVar)
solve [] = pure mempty
solve (x:xs) = case x of
  TyEq a b -> do
    m <- mgu a b
    rest <- solve (fmap (fmap (apply m)) xs)
    pure (rest `comp` m)
  TyInst a b -> do
    t <- instantiate b
    solve (TyEq a t : xs)

mkConstraints :: TypeEnv -> [Assumption] -> [Constraint]
mkConstraints TypeEnv{unTypeEnv} = mapMaybe mkC where
  mkC :: Assumption -> Maybe Constraint
  mkC (sqlVar, sqlType) = case Map.lookup sqlVar unTypeEnv of
    Nothing -> Nothing
    Just x  -> Just (TyInst sqlType x)

data InferError =
  IAnnotateError AnnotateErr
  | IUnificationError (UnificationError TyVar)
  deriving (Eq, Show)

{-| Infer the type of a scalar expression under the given type env
-}
runInferType :: TypeEnv -> ScalarExpr -> Either InferError (Substitution TyVar, SqlType TyVar, [Assumption])
runInferType env = runIdentity . runExceptT . evalFreshT . inferType env

{-| Infer the type of a scalar expression
-}
inferType :: (MonadError InferError m, MonadFresh m) => TypeEnv -> ScalarExpr -> m (Substitution TyVar, SqlType TyVar, [Assumption])
inferType typeEnv expr = do
  (tp, (assumptions, constraints)) <- runExceptT (runAnnotateT (cataA typeVars expr)) >>= either (throwError . IAnnotateError) pure
  let constraints' = mkConstraints typeEnv assumptions ++ constraints
  subs <- runExceptT (solve constraints') >>= either (throwError . IUnificationError) pure
  return (subs, apply subs tp, fmap (second (apply subs)) assumptions)

data UnificationError v =
  UnificationError (SqlType v) (SqlType v)
  deriving (Eq, Show)

{-| most general unifier of two types. If @mgu a b == Just s@ then it
should hold that @apply s a == apply s b@.

See Samuel R. Buss: Chapter I - An Introduction to Proof Theory in Handbook of Proof Theory (Elsevier 1998)
-}
mgu :: forall m v. (MonadError (UnificationError v) m, Ord v) => SqlType v -> SqlType v -> m (Substitution v)
mgu a b = go (a, b) where
  go :: (SqlType v, SqlType v) -> m (Substitution v)
  go = \case
    (STVar a', STVar b') | a' == b' -> pure mempty
    (STVar a', term)
      | a' `elem` freeVars term -> throwError (UnificationError (STVar a') term)
      | otherwise -> pure (singleton a' term)
    (term, STVar a')
      | a' `elem` freeVars term -> throwError (UnificationError term (STVar a'))
      | otherwise -> pure (singleton a' term)
    (STArr a' b', STArr c' d') -> do
      subs' <- go (a', c')
      k <- go (apply subs' b', apply subs' d')
      pure (subs' <> k)
    (x, y) | x == y    -> pure mempty
           | otherwise -> throwError (UnificationError x y)

{-| Function type with a list of arguments and a result
-}
arr :: [SqlType a] -> SqlType a -> SqlType a
arr []     r = r
arr (x:xs) r = STArr x (arr xs r)

data AnnotateErr =
  UnsupportedScalarExpr String
  deriving (Eq, Ord, Show)

type Annotate = AnnotateT (FreshT (ExceptT AnnotateErr Identity))

runAnnotate :: Annotate a -> Either AnnotateErr (a, ([Assumption], [Constraint]))
runAnnotate = runIdentity . runExceptT . evalFreshT . runAnnotateT

{-| Generate a fresh type variable for a term-level variable
-}
bindVar :: (MonadAnnotate m, MonadFresh m) => SqlVar -> m (SqlType TyVar)
bindVar v = do
  tv <- STVar <$> freshVar
  write ([(v, tv)], [])
  pure tv

{-| Emit constraints for an operator of the given name
-}
operator :: (MonadFresh m, MonadAnnotate m) =>
  [Name]
  -- ^ Name of the operator
  -> [SqlType TyVar]
  -- ^ Types of the operator's arguments
  -> m (SqlType TyVar)
  -- ^ Type variable for the operator type
operator names tps = do
  result <- STVar <$> freshVar
  write ([(AnOperator names, arr tps result)], [])
  pure result

{-| Emit constraints declaring the provided types equal
-}
allEquals :: MonadAnnotate m => [SqlType TyVar] -> m ()
allEquals tps =
  let cons = zipWith TyEq tps (drop 1 tps)
  in write ([], cons)

typeConstraints :: ScalarExpr -> Either AnnotateErr (SqlType TyVar, ([Assumption], [Constraint]))
typeConstraints = runAnnotate . cataA typeVars

{-| Produce the type variables and assumptions for a scalar expression.
-}
typeVars :: (MonadError AnnotateErr m, MonadAnnotate m, MonadFresh m) => ScalarExprF (m (SqlType TyVar)) -> m (SqlType TyVar)
typeVars = \case
  NumLit{}                 -> pure STNumber
  StringLit{}              -> pure STText
  IntervalLit{}            -> pure STInterval
  TypedLit tn _            -> pure (STSqlType tn)
  Iden names               -> bindVar (AnIdentifier names)
  PositionalArg i          -> bindVar (APosArg i)
  HostParameter a b        -> bindVar (AHostParameter a b)
  BinOp x1 opNames x2      -> sequence [x1, x2] >>= operator opNames
  PrefixOp opNames x       -> x >>= operator opNames . return
  PostfixOp opNames x      -> x >>= operator opNames . return
  SpecialOp names args     -> sequence args >>= operator names
  App names args           -> sequence args >>= operator names
  Parens x                 -> x
  Cast x (STSqlType -> tn) ->
     -- TODO: do we need to check that the cast is legit? (This probably depends on the SQL dialect)
    x >> pure tn
  In _ b (InList bs)       -> sequence (b:bs) >>= allEquals >> pure STBool
  k                        -> throwError $ UnsupportedScalarExpr (show $ void k)
