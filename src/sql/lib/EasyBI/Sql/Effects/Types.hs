{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE ViewPatterns       #-}
module EasyBI.Sql.Effects.Types(
  TyVar(..),
  Assumption,
  Constraint,
  SqlVar(..),
  SqlType(..),
  freeVars,
  TyScheme(..),
  freeVarsS,
  TyConstraint(..),
  Substitution,
  apply,
  singleton,
  fromList,
  comp,
  TypeEnv(..)
  ) where

import           Data.Foldable                 (toList)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           EasyBI.Sql.Syntax             ()
import           Language.SQL.SimpleSQL.Syntax (Name, TypeName)

{-| Type variables
-}
newtype TyVar = TyVar Int
  deriving stock (Eq, Ord, Show)
  deriving newtype Num

type Assumption = (SqlVar, SqlType TyVar)

type Constraint = TyConstraint TyVar (SqlType TyVar)

{-| There are two (syntactically) different kinds of variables,
corresponding to the @PositionalArg@ and @HostParameter@ constructors
of 'ScalarExpr'
-}
data SqlVar =
  APosArg Int
  | AHostParameter String (Maybe String)
  | AnIdentifier [Name]
  | AnOperator [Name]
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

{-| The set of free type variables of a type @t@ is denoted by @freevars t@ and simply
consists of all type variables in @t@.
-}
freeVars :: SqlType v -> [v]
freeVars = toList

{-| Free variables in a type scheme
-}
freeVarsS :: Ord v => TyScheme v (SqlType v) -> [v]
freeVarsS (TyScheme (Set.fromList -> s) t) =
  filter (\t' -> not (t' `Set.member` s)) (freeVars t)


{-|
-}
data TyConstraint v t =
  TyEq t t -- ^ Equality
  | TyInst t (TyScheme v t) -- ^ An explicit instance constraint @a `TyInst` b@ means @a@ has to be a generic instance of @b@
  -- | TyImplInst v v -- ^ Implicit instance constraint (see paper) - we don't need it (?)
  deriving stock (Eq, Show, Foldable, Functor, Traversable)

data TyScheme v t = TyScheme [v] t
  deriving stock (Eq, Show, Foldable, Functor, Traversable)

{-| A substitution is a mapping of type variable to types.
-}
newtype Substitution v = Substitution { unSubst :: Map v (SqlType v) }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

singleton :: v -> SqlType v -> Substitution v
singleton k = Substitution . Map.singleton k

fromList :: Ord v => [(v, SqlType v)] -> Substitution v
fromList = Substitution . Map.fromList

delete :: Ord v => v -> Substitution v -> Substitution v
delete k (Substitution m) = Substitution (Map.delete k m)

{-| Apply a substitution to a type
-}
apply :: Ord v => Substitution v -> SqlType v -> SqlType v
apply s@Substitution{unSubst} = \case
  STVar v   -> case Map.lookup v unSubst of
    Nothing -> STVar v
    Just v' -> apply (delete v s) v'
  STArr l r -> STArr (apply s l) (apply s r)
  x         -> x

{-| Composition of two substititions
@apply (comp s2 s1) a == apply s2 (apply s1 a)@
-}
comp :: Ord v => Substitution v -> Substitution v -> Substitution v
comp (Substitution s2) (Substitution s1) =

  -- s1 followed by s2
  let k = fmap (apply (Substitution s2)) s1
  in Substitution $ k `Map.union` (Map.difference s2 s1)

newtype TypeEnv = TypeEnv { unTypeEnv :: Map SqlVar (TyScheme TyVar (SqlType TyVar)) }
