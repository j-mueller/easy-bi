{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-| Typing SQL statements. Based on "Generalizing Hindley-Milner Type Inference Algorithms" by B. Heeren, J. Hage and D. Swierstra (technical report)
-}
module EasyBI.Sql.Types(
  TyVar(..),
  SqlType(..),
  SqlVar(..),
  TyConstraint(..),
  AnnotateErr(..),
  typeConstraints,

  -- * Substitutions and unifiers
  Substitution,
  apply,
  mgu,
  singleton
) where

import           Control.Monad                 (void)
import           Control.Monad.Except          (ExceptT, MonadError (..),
                                                runExceptT)
import           Control.Monad.State.Strict    (MonadState (..), StateT,
                                                runStateT)
import           Control.Monad.Writer          (WriterT, runWriterT, tell)
import           Data.Bifunctor                (Bifunctor (..))
import           Data.Foldable                 (toList)
import           Data.Functor.Foldable         (cataA)
import           Data.Functor.Identity         (Identity (..))
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           Data.Maybe                    (fromMaybe)
import           EasyBI.Sql.Syntax             (ScalarExprF (..))
import           Language.SQL.SimpleSQL.Syntax (Name, ScalarExpr, TypeName)

{-| Type variables
-}
newtype TyVar = TyVar Int
  deriving stock (Eq, Ord, Show)
  deriving newtype Num

data SqlType v =
  STVar v
  | STNumber
  | STText
  | STBool
  | STArr (SqlType v) (SqlType v)
  | STInterval
  | STSqlType TypeName
  deriving stock (Eq, Show, Foldable, Functor, Traversable)

{-| The set of free type variables of a type @t@ is denoted by @freevars t@ and simply
consists of all type variables in @t@.
-}
freeVars :: SqlType v -> [v]
freeVars = toList

{-| A substitution is a mapping of type variable to types.
-}
newtype Substitution v = Substitution { unSubst :: Map v (SqlType v) }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

singleton :: v -> SqlType v -> Substitution v
singleton k = Substitution . Map.singleton k

{-| Apply a substitution to a type
-}
apply :: Ord v => Substitution v -> SqlType v -> SqlType v
apply s@Substitution{unSubst} = \case
  STVar v   -> fromMaybe (STVar v) (Map.lookup v unSubst)
  STArr l r -> STArr (apply s l) (apply s r)
  x         -> x

{-| most general unifier of two types. If @mgu a b == Just s@ then it
should hold that @apply s a == apply s b@.

See Samuel R. Buss: Chapter I - An Introduction to Proof Theory in Handbook of Proof Theory (Elsevier 1998)
-}
mgu :: forall v. Ord v => SqlType v -> SqlType v -> Maybe (Substitution v)
mgu a b = go (a, b) where
  go :: (SqlType v, SqlType v) -> Maybe (Substitution v)
  go = \case
    (STVar a', STVar b') | a' == b' -> Just mempty
    (STVar a', term)
      | a' `elem` freeVars term -> Nothing
      | otherwise -> Just (singleton a' term)
    (term, STVar a')
      | a' `elem` freeVars term -> Nothing
      | otherwise -> Just (singleton a' term)
    (STArr a' b', STArr c' d') -> do
      subs' <- go (a', c')
      k <- go (apply subs' b', apply subs' d')
      pure (subs' <> k)
    (x, y) | x == y    -> Just mempty
           | otherwise -> Nothing

{-| Function type with a list of arguments and a result
-}
arr :: [SqlType a] -> SqlType a -> SqlType a
arr []     r = r
arr (x:xs) r = STArr x (arr xs r)

{-| There are two (syntactically) different kinds of variables,
corresponding to the @PositionalArg@ and @HostParameter@ constructors
of 'ScalarExpr'
-}
data SqlVar =
  APosArg Int
  | AHostParameter String (Maybe String)
  | AnIdentifier [Name]
  | AnOperator [Name]
  deriving stock (Eq, Show)

{-|
-}
data TyConstraint v =
  TyEq v v -- ^ Equality
  | TyInst v v -- ^ An explicit instance constraint @a `TyInst` b@ means @a@ has to be a generic instance of @b@
  -- | TyImplInst v v -- ^ Implicit instance constraint (see paper) - we don't need it (?)
  deriving stock (Eq, Ord, Show, Foldable, Functor, Traversable)

newtype TyVarState = TyVarState Int
  deriving stock (Eq, Ord, Show)

newtype AnnotateT m a = AnnotateT{ unAnnotateT :: StateT TyVarState (WriterT ([Assumption], [Constraint]) (ExceptT AnnotateErr m)) a }
  deriving newtype (Functor, Applicative, Monad)

type Assumption = (SqlVar, SqlType TyVar)

type Constraint = TyConstraint (SqlType TyVar)

class Monad m => MonadInfer m where
  freshVar :: m TyVar
  write :: ([Assumption], [Constraint]) -> m ()

instance Monad m => MonadFail (AnnotateT m) where
  fail = AnnotateT . throwError . UnsupportedScalarExpr

instance Monad m => MonadInfer (AnnotateT m) where
  freshVar = AnnotateT $ do
    TyVarState i <- get
    put (TyVarState $ succ i)
    return (TyVar i)
  write = AnnotateT . tell

data AnnotateErr =
  UnsupportedScalarExpr String
  deriving (Eq, Ord, Show)

runAnnotateT :: AnnotateT m a -> m (Either AnnotateErr ((a, TyVarState), ([Assumption], [Constraint])))
runAnnotateT = runExceptT . runWriterT . flip runStateT (TyVarState 0) . unAnnotateT

type Annotate = AnnotateT Identity

runAnnotate :: Annotate a -> Either AnnotateErr ((a, TyVarState), ([Assumption], [Constraint]))
runAnnotate = runIdentity . runAnnotateT

{-| Generate a fresh type variable for a term-level variable
-}
bindVar :: MonadInfer m => SqlVar -> m (SqlType TyVar)
bindVar v = do
  tv <- STVar <$> freshVar
  write ([(v, tv)], [])
  pure tv

{-| Emit constraints for an operator of the given name
-}
operator :: MonadInfer m =>
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

typeConstraints :: ScalarExpr -> Either AnnotateErr (SqlType TyVar, ([Assumption], [Constraint]))
typeConstraints = fmap (first fst) . runAnnotate . cataA typeVars

{-| Produce the type variables and assumptions for a scalar expression.
-}
typeVars :: (MonadFail m, MonadInfer m) => ScalarExprF (m (SqlType TyVar)) -> m (SqlType TyVar)
typeVars = \case
  NumLit{}             -> pure STNumber
  StringLit{}          -> pure STText
  IntervalLit{}        -> pure STInterval
  TypedLit tn _        -> pure (STSqlType tn)
  Iden names           -> bindVar (AnIdentifier names)
  PositionalArg i      -> bindVar (APosArg i)
  HostParameter a b    -> bindVar (AHostParameter a b)
  BinOp x1 opNames x2  -> sequence [x1, x2] >>= operator opNames
  PrefixOp opNames x   -> x >>= operator opNames . return
  PostfixOp opNames x  -> x >>= operator opNames . return
  SpecialOp names args -> sequence args >>= operator names
  App names args       -> sequence args >>= operator names
  k                    -> fail (show $ void k)
