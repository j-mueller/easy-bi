{-# LANGUAGE FlexibleContexts #-}
{-| Classes for treating SQL fragments in a uniform way
-}
module EasyBI.Sql.Class
  ( SqlFragment (..)
  , inferType
  , runInferType
  , typeConstraints
    -- * Re-exported dialects
  , sqlite
  ) where

import Control.Monad.Except           (ExceptT, MonadError (throwError),
                                       runExceptT)
import Data.Functor.Foldable          (cataA)
import Data.Functor.Identity          (Identity (..))
import Data.Map.Strict                qualified as Map
import EasyBI.Sql.Dialect             (sqlite)
import EasyBI.Sql.Effects.Annotate    (AnnotateT, MonadAnnotate, runAnnotateT)
import EasyBI.Sql.Effects.Fresh       (FreshT, MonadFresh, evalFreshT)
import EasyBI.Sql.Effects.Types       (Assumption, Constraint, SqlVar,
                                       Substitution, Tp, TyVar, TypeEnv)
import EasyBI.Sql.Types               (AnnotateErr, InferError (..))
import EasyBI.Sql.Types               qualified as Types
import Language.SQL.SimpleSQL.Dialect (Dialect)
import Language.SQL.SimpleSQL.Parse   (ParseError)
import Language.SQL.SimpleSQL.Parse   qualified as Parse
import Language.SQL.SimpleSQL.Pretty  qualified as Pretty
import Language.SQL.SimpleSQL.Syntax  (QueryExpr, ScalarExpr)

{-| Fragments of SQL that we can parse, render, and infer types for
-}
class SqlFragment a where
  -- | Parse a SQL fragment from a string
  parse :: Dialect -> FilePath -> Maybe (Int, Int) -> String -> Either ParseError a

  -- | Generate type annotations for the SQL fragment
  annotate :: (MonadError AnnotateErr m, MonadAnnotate m, MonadFresh m) => a -> m (Tp TyVar)

  -- | Render a sql statement to a string
  render :: Dialect -> a -> String

instance SqlFragment ScalarExpr where
  parse  = Parse.parseScalarExpr
  annotate = cataA Types.annotScalarExprF
  render = Pretty.prettyScalarExpr

instance SqlFragment QueryExpr where
  parse  = Parse.parseQueryExpr
  annotate = cataA Types.annotQueryExprF
  render = Pretty.prettyQueryExpr

{-| Infer the type of a SQL fragment
-}
runInferType :: SqlFragment e => TypeEnv -> e -> Either InferError (Substitution TyVar, Tp TyVar, Map.Map SqlVar (Tp TyVar))
runInferType env = runIdentity . runExceptT . evalFreshT . inferType env

{-| Produce the type constraints for the SQL fragment
-}
typeConstraints :: SqlFragment e => e -> Either AnnotateErr (Tp TyVar, ([Assumption], [Constraint]))
typeConstraints = runAnnotate . annotate

{-| Infer the type of a SQL fragment with effects
-}
inferType :: (MonadError InferError m, MonadFresh m, SqlFragment a) => TypeEnv -> a -> m (Substitution TyVar, Tp TyVar, Map.Map SqlVar (Tp TyVar))
inferType typeEnv expr = do
  (tp, (assumptions, constraints)) <- runExceptT (runAnnotateT (annotate expr)) >>= either (throwError . IAnnotateError) pure
  (constraints', assignments) <- Types.mkConstraints typeEnv assumptions
  let allConstraints = constraints <> constraints'
  subs <- runExceptT (Types.solve allConstraints) >>= either (throwError . IUnificationError allConstraints) pure
  return (subs, Types.apply subs tp, fmap (Types.apply subs) assignments)

type Annotate = AnnotateT (FreshT (ExceptT AnnotateErr Identity))

runAnnotate :: Annotate a -> Either AnnotateErr (a, ([Assumption], [Constraint]))
runAnnotate = runIdentity . runExceptT . evalFreshT . runAnnotateT
