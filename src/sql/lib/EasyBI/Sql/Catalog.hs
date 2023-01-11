{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-| Information about a database
-}
module EasyBI.Sql.Catalog
  ( Catalog (..)
  , addStatement
  , inferTypeCat
  , tables
  , views
  ) where

import Control.Lens                  (at, makeLenses, (.=))
import Control.Monad.Except          (MonadError (throwError))
import Control.Monad.State.Strict    (MonadState, get)
import Data.Map.Strict               (Map)
import EasyBI.Sql.Class              (SqlFragment, runInferType)
import EasyBI.Sql.Effects.Types      (SqlType, SqlVar (..), Tp, TyScheme, TyVar,
                                      TypeEnv (..), generalise)
import EasyBI.Sql.Types              (InferError, defaultTypeEnv, rowFromSchema)
import Language.SQL.SimpleSQL.Syntax (Name, QueryExpr, Statement (..))

{-| List of database objects that we know about
-}
data Catalog =
  Catalog
    {

    -- | Database tables that we know about.
    _tables  :: Map SqlVar (TyScheme TyVar (Tp TyVar))

    -- | Views that we can show to the user
    -- They are predefined SQL queries that can be used
    -- as the basis for views in the UI.
    -- They don't correspond to actual database views.
    , _views :: Map [Name] (QueryExpr, Tp TyVar)
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
  CreateView _ names _ queryExpr _ -> do
    tp <- get >>= flip inferTypeCat queryExpr
    views . at names .= Just (queryExpr, tp)
    pure (Just $ generalise tp)
  _ -> pure Nothing
