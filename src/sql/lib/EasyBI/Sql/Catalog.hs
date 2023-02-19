{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-| Information about a database
-}
module EasyBI.Sql.Catalog
  ( Catalog (..)
  , TypedQueryExpr (..)
  , addStatement
  , fromStatements
  , inferTypeCat
  , tables
  , views
  ) where

import Codec.Serialise               (Serialise)
import Control.Lens                  (at, makeLenses, (.=))
import Control.Monad.Except          (MonadError (throwError), runExcept)
import Control.Monad.State.Strict    (MonadState, get, runStateT)
import Data.Aeson                    (FromJSON, ToJSON)
import Data.Bifunctor                (Bifunctor (..))
import Data.Map.Strict               (Map)
import Data.Maybe                    (catMaybes)
import EasyBI.Sql.Class              (SqlFragment, runInferType)
import EasyBI.Sql.Effects.Types      (SqlType, SqlVar (..), Tp, TyScheme, TyVar,
                                      TypeEnv (..), generalise)
import EasyBI.Sql.Types              (InferError, defaultTypeEnv, rowFromSchema)
import GHC.Generics                  (Generic)
import Language.SQL.SimpleSQL.Syntax (Name, QueryExpr, Statement (..))

data TypedQueryExpr =
  TypedQueryExpr
    { teQuery :: QueryExpr
    , teType  :: TyScheme TyVar (Tp TyVar)
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON)

{-| List of database objects that we know about
-}
data Catalog =
  Catalog
    {

    -- | Database tables that we know about.
    _tables  :: Map SqlVar (TyScheme TyVar (Tp TyVar))

    -- | Views that can be used
    -- as the basis for cubes in the UI.
    -- They don't correspond to actual database views.
    , _views :: Map [Name] TypedQueryExpr
    }

makeLenses ''Catalog

instance Semigroup Catalog where
  l <> r =
    Catalog
      { _tables = _tables l <> _tables r
      , _views = _views l <> _views r
      }

instance Monoid Catalog where
  mempty = Catalog mempty mempty

{-| Infer the type of a SQL fragment using the information from the 'Catalog'
-}
inferTypeCat :: (MonadError InferError m, SqlFragment x) => Catalog -> x -> m (Tp TyVar)
inferTypeCat Catalog{_tables} fragment =
  case runInferType ((TypeEnv _tables) <> defaultTypeEnv) fragment of
    Left err         -> throwError err
    Right (_, tp, _) -> pure tp

{-| Add a 'Statement' to the catalog and return the inferred type if the statement
was a CREATE VIEW statement or a CREATE TABLE statement
-}
addStatement :: (MonadError InferError m, MonadState Catalog m) => Map String SqlType -> Statement -> m (Maybe (TyScheme TyVar (Tp TyVar)))
addStatement typeOverrides = \case
  CreateTable names elements -> do
    let tp = rowFromSchema elements typeOverrides
    tables . at (AnIdentifier names) .= Just tp
    pure (Just tp)
  CreateView _ names _ teQuery _ -> do
    teType <- get >>= fmap generalise . flip inferTypeCat teQuery
    views . at names .= Just TypedQueryExpr{teQuery, teType}
    pure (Just teType)
  _ -> pure Nothing

{-| Create a catalog from a list of statements, returning the catalog and a list
of the inferred types of all views and tables
-}
fromStatements ::
  Map String SqlType ->
    -- ^ Manual type overrides for columns
  [Statement] ->
    -- ^ List of statements
  Either InferError ([TyScheme TyVar (Tp TyVar)], Catalog)
fromStatements overrides = fmap (first catMaybes) . runExcept . flip runStateT mempty . traverse (addStatement overrides)
