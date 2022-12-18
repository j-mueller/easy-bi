{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
module EasyBI.Sql.Effects.Types(
  TyVar(..),
  Assumption,
  Constraint,
  SqlVar(..),
  SqlType(..),
  RowType(..),
  mkRow,
  Tp(..),
  freeVars,
  TyScheme(..),
  freeVarsS,
  TyConstraint(..),
  Substitution,
  apply,
  applyCons,
  singleton,
  fromList,
  insertRow,
  comp,
  TypeEnv(..),
  InferenceLog(..)
  ) where

import           Data.Foldable                 (toList)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           EasyBI.Sql.Syntax             ()
import           Language.SQL.SimpleSQL.Syntax (Name (..), TypeName)
import           Prettyprinter                 (Doc, Pretty (..), comma,
                                                concatWith, viaShow, (<+>))

{-| Type variables
-}
newtype TyVar = TyVar Int
  deriving stock (Eq, Ord, Show)
  deriving newtype Num

instance Pretty TyVar where
  pretty (TyVar v) = "v" <> pretty v

type Assumption = (SqlVar, Tp TyVar)

type Constraint = TyConstraint TyVar (Tp TyVar)

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

instance Pretty SqlVar where
  pretty = \case
    APosArg i -> "$" <> pretty i
    AHostParameter nm _ -> pretty nm
    AnIdentifier names -> concatWith (\a b -> a <> "." <> b) (prettyName <$> names)
    AnOperator names -> concatWith (\a b -> a <> "." <> b) (prettyName <$> names)

prettyName :: forall ann. Name -> Doc ann
prettyName (Name Nothing nm)       = pretty nm
prettyName (Name (Just (a, b)) nm) = pretty a <> pretty nm <> pretty b

{-| Row type with a type variable for the "rest of the row"
-}
data RowType v = RowType (Tp v) (Map Name (Tp v))
  deriving stock (Eq, Show, Foldable, Functor, Traversable)

instance Pretty v => Pretty (RowType v) where
  pretty = \case
    RowType tp (Map.toList -> names) ->
      let labels = fmap (\(nm, tp') -> prettyName nm <> ":" <+> pretty tp') names
          names' = concatWith (\a b -> a <> comma <+> b) labels
      in "{" <> names' <> if null names then mempty else "," <+> pretty tp <> "}"

mkRow :: v -> [(Name, Tp v)] -> RowType v
mkRow v vls =
  RowType (TpVar v) $ Map.fromList vls

{-| Universe of types, containing both row types and regular types
-}
data Tp v =
  TpSql SqlType
  | TpRow (RowType v)
  | TpArr (Tp v) (Tp v)
  | TpVar v
  deriving stock (Eq, Show, Foldable, Functor, Traversable)

instance Pretty v => Pretty (Tp v) where
  pretty = \case
    TpSql t   -> pretty t
    TpRow t   -> pretty t
    TpArr l r -> pretty l <+> "→" <+> pretty r
    TpVar v   -> pretty v

data SqlType =
  STNumber
  | STText
  | STBool
  | STInterval
  | STSqlType TypeName
  deriving stock (Eq, Show)

instance Pretty SqlType where
  pretty = \case
    STNumber     -> "number"
    STText       -> "text"
    STBool       -> "bool"
    STInterval   -> "interval"
    STSqlType tn -> "<" <> viaShow tn <> ">"

{-| The set of free type variables of a type @t@ is denoted by @freevars t@ and simply
consists of all type variables in @t@.
-}
freeVars :: Foldable f => f v -> [v]
freeVars = toList

{-| Free variables in a type scheme
-}
freeVarsS :: (Foldable f, Ord v) => TyScheme v (f v) -> [v]
freeVarsS (TyScheme (Set.fromList -> s) t) =
  filter (\t' -> not (t' `Set.member` s)) (freeVars t)

{-|
-}
data TyConstraint v t =
  TyEq t t -- ^ Equality
  | TyInst t (TyScheme v t) -- ^ An explicit instance constraint @a `TyInst` b@ means @a@ has to be a generic instance of @b@
  -- | TyImplInst v v -- ^ Implicit instance constraint (see paper) - we don't need it (?)
  deriving stock (Eq, Show, Foldable, Functor, Traversable)

instance (Pretty v, Pretty t) => Pretty (TyConstraint v t) where
  pretty = \case
    TyEq l r   -> pretty l <+> "≡" <+> pretty r
    TyInst t s -> pretty t <+> "≤" <+> pretty s

data TyScheme v t = TyScheme [v] t
  deriving stock (Eq, Show, Foldable, Functor, Traversable)

instance (Pretty v, Pretty t) => Pretty (TyScheme v t) where
  pretty (TyScheme [] ts) = pretty ts
  pretty (TyScheme xs ts) =
    let vs = "∀" <+> concatWith (\a b -> a <> comma <+> b) (pretty <$> xs)
    in vs <> "." <+> pretty ts

{-| A substitution is a mapping of type variable to types.
-}
newtype Substitution v =
  -- we need to distinguish by kind here. otherwise 'apply' is only partial
  Substitution{ subsType :: Map v (Tp v) }
  deriving stock (Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance Pretty v => Pretty (Substitution v) where
  pretty (Substitution (Map.toList -> ls)) =
    let ls' = fmap (\(v, t) -> pretty v <+> "↦" <+> pretty t) ls
    in concatWith (\a b -> a <> comma <+> b) ls'

singleton :: v -> Tp v -> Substitution v
singleton k v = Substitution (Map.singleton k v)

lkp :: Ord v => v -> Substitution v -> Maybe (Tp v)
lkp k (Substitution s) = Map.lookup k s

fromList :: Ord v => [(v, Tp v)] -> Substitution v
fromList xs = Substitution (Map.fromList xs)

insertRow :: Ord v => v -> RowType v -> Substitution v -> Substitution v
insertRow k v Substitution{subsType} =
  Substitution (Map.insert k (TpRow v) subsType)

delete :: Ord v => v -> Substitution v -> Substitution v
delete k (Substitution m) = Substitution (Map.delete k m)

applyCons :: Substitution TyVar -> TyConstraint TyVar (Tp TyVar) -> TyConstraint TyVar (Tp TyVar)
applyCons subs = \case
  TyEq t1 t2  -> TyEq (apply subs t1) (apply subs t2)
  TyInst t1 (TyScheme vars t2) ->
    let subs' = foldr delete subs vars
    in TyInst (apply subs t1) (TyScheme vars (apply subs' t2))

apply :: (Show v, Ord v) => Substitution v -> Tp v -> Tp v
apply subs = \case
  TpSql t   -> TpSql t
  TpRow r   -> TpRow (applyRow subs r)
  TpArr l r -> TpArr (apply subs l) (apply subs r)
  TpVar v   -> case lkp v subs of
    Just v' -> apply (delete v subs) v'
    Nothing -> TpVar v

{-| Apply a substitution to a row type
-}
applyRow :: (Show v, Ord v) => Substitution v -> RowType v -> RowType v
applyRow s@Substitution{subsType} (RowType (TpVar v) mp) =
  case Map.lookup v subsType of
    Nothing -> RowType (TpVar v) (fmap (apply s) mp)
    -- TODO: Is this the right thing to do?
    Just (TpRow (RowType v'' mp')) -> applyRow (delete v s) (RowType v'' $ Map.unionWith (\a b -> error $ "applyRow: error " <> show a <> " - " <> show b) mp mp')
    Just (TpVar v') -> applyRow (delete v s) (RowType (TpVar v') mp)
    Just t -> error $ "applyRow: found unxpected type " <> show t
applyRow _ (RowType t _) = error $ "applyRow: Unexpected type in row: " <> show t

{-| Composition of two substitutions
@apply (comp s2 s1) a == apply s2 (apply s1 a)@
-}
comp :: (Show v, Ord v) => Substitution v -> Substitution v -> Substitution v
comp (Substitution s2) (Substitution s1) =

  let -- s1 followed by s2
      s3 = fmap (apply (Substitution s2)) s1

  in Substitution
      { subsType = s3 `Map.union` (Map.difference s2 s1)
      }

newtype TypeEnv = TypeEnv { unTypeEnv :: Map SqlVar (TyScheme TyVar (Tp TyVar)) }

data InferenceLog =
  Instantiate (TyScheme TyVar (Tp TyVar)) (Tp TyVar)
  deriving Show

instance Pretty InferenceLog where
  pretty (Instantiate scheme tp) =
    "Instantiating" <+> pretty scheme <+> "to" <+> pretty tp
