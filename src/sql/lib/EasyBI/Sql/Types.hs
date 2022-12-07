{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-| Typing SQL statements
-}
module EasyBI.Sql.Types(
  TyVar(..),
  SqlType(..),
  SqlVar(..),
  TyConstraint(..),
  AnnotateErr(..),
  typeConstraints
) where

import           Control.Monad                 (void)
import           Control.Monad.Except          (ExceptT, MonadError (..),
                                                runExceptT)
import           Control.Monad.State.Strict    (MonadState (..), StateT,
                                                runStateT)
import           Control.Monad.Writer          (WriterT, runWriterT, tell)
import           Data.Bifunctor                (Bifunctor (..))
import           Data.Functor.Foldable         (cataA)
import           Data.Functor.Identity         (Identity (..))
import           EasyBI.Sql.Syntax             (ScalarExprF (..))
import           Language.SQL.SimpleSQL.Syntax (Name, ScalarExpr, TypeName)

{-| Type variables
-}
newtype TyVar = TyVar Int
  deriving stock (Eq, Ord, Show)

data SqlType v =
  STVar v
  | STNumber
  | STText
  | STBool
  | STArr (SqlType v) (SqlType v)
  | STInterval
  | STSqlType TypeName
  deriving stock (Eq, Show, Foldable, Functor, Traversable)

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
  | TyInst v v -- ^ @a `TyInst` b@ means @a@ has to be a generic instance of @b@
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
typeConstraints = fmap (first fst) . runAnnotate . cataA ex where
  ex = \case
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
