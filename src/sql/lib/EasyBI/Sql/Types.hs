{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE ViewPatterns       #-}
{-| Typing SQL statements. Based on "Generalizing Hindley-Milner Type Inference Algorithms" by B. Heeren, J. Hage and D. Swierstra (technical report)
-}
module EasyBI.Sql.Types
  ( AnnotateErr (..)
  , SqlType (..)
  , SqlVar (..)
  , TyConstraint (..)
  , TyVar (..)
  , UnificationError (..)
  , getFailure
  , rowFromSchema
  , typeConstraints
    -- * Type enviroments
  , TypeEnv (..)
  , defaultTypeEnv
    -- * Substitutions and unifiers
  , InferError (..)
  , Substitution
  , apply
  , comp
  , inferType
  , mgu
  , runInferType
  , singleton
  ) where

import Control.Lens                  (_1, _2, _3, assign, modifying, use)
import Control.Monad                 (void)
import Control.Monad.Except          (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State.Strict    (execStateT)
import Control.Monad.Trans.Class     (MonadTrans (..))
import Control.Monad.Writer          (runWriterT)
import Data.Bifunctor                (Bifunctor (..))
import Data.Either                   (partitionEithers)
import Data.Functor.Foldable         (cataA)
import Data.Functor.Identity         (Identity (..))
import Data.Map                      (Map)
import Data.Map.Merge.Strict         qualified as Merge
import Data.Map.Strict               qualified as Map
import Data.Maybe                    (mapMaybe)
import EasyBI.Sql.BuiltinTypes       (defaultTypeEnv)
import EasyBI.Sql.Effects.Annotate   (AnnotateT, MonadAnnotate (..),
                                      runAnnotateT)
import EasyBI.Sql.Effects.Fresh      (FreshT, MonadFresh (..), evalFreshT,
                                      instantiate)
import EasyBI.Sql.Effects.Types      (Assumption, Constraint, InferenceLog (..),
                                      RowType (..), SqlType (..), SqlVar (..),
                                      Substitution, Tp (..), TyConstraint (..),
                                      TyScheme (..), TyVar (..), TypeEnv (..),
                                      apply, applyCons, comp, freeVars,
                                      fromTypeName, insertRow, mkRow, singleton)
import EasyBI.Sql.Syntax             (InPredValueF (..), ScalarExprF (..))
import Language.SQL.SimpleSQL.Syntax (ColumnDef (..), Name (..), ScalarExpr,
                                      TableElement (..))
import Prettyprinter                 (Pretty (..), colon, hang, viaShow, vsep,
                                      (<+>))

{-| Turn a list of 'TableElement's (from a CREATE TABLE statement)
into a row type
-}
rowFromSchema ::
  [TableElement] ->
  Map String SqlType ->
  TyScheme TyVar (Tp TyVar)
rowFromSchema elms customTypes =
  let rowVar = 0
      elements = flip mapMaybe elms $ \case
        TableColumnDef (ColumnDef nm (TpSql . fromTypeName -> tp) _ _) ->
          let Name _ nm' = nm
          in case Map.lookup nm' customTypes of
              Nothing  -> Just (nm, tp)
              Just tp' -> Just (nm, TpSql tp')
        _                                          -> Nothing
      rowTp = RowType (TpVar rowVar) (Map.fromList elements)
  in TyScheme [rowVar] (TpRow rowTp)

solve :: (MonadError (UnificationError TyVar) m, MonadFresh m) => [TyConstraint TyVar (Tp TyVar)] -> m (Substitution TyVar)
solve [] = pure mempty
solve (x:xs) = case x of
  TyEq a b -> do
    m <- wrapError (MguGoal x) (mgu a b)
    rest <- solve (fmap (applyCons m) xs)
    pure (rest `comp` m)
  TyInst a b -> do
    (t, e) <- runWriterT (instantiate b)
    case e of
      [Instantiate k y] -> wrapError (MguInstantiate k y) (solve (TyEq a t : xs))
      _ -> error "solve: unexpected inference log"

{-| Turn the 'Assumption's into 'Constraint's
-}
mkConstraints :: MonadFresh m => TypeEnv -> [Assumption] -> m ([Constraint], Map.Map SqlVar (Tp TyVar))
mkConstraints _ []                                        = pure ([], mempty)
mkConstraints e@TypeEnv{unTypeEnv} ((sqlVar, sqlType):xs) =
  case Map.lookup sqlVar unTypeEnv of
    Just x -> first ((:) (TyInst sqlType x)) <$> mkConstraints e xs
    Nothing -> do
      v <- TpVar <$> freshVar
      let applicableConstraint (sqlVar', sqlType')
            | sqlVar' == sqlVar = Left sqlType'
            | otherwise         = Right (sqlVar', sqlType')

          -- TODO: Use a map (SqlVar, Assumption) instead of []
          (applicableConstraints, rest) = partitionEithers (applicableConstraint <$> xs)

          constraintsFromAssumptions :: [Constraint]
          constraintsFromAssumptions = TyEq v sqlType : fmap (TyEq v) applicableConstraints


          result = constraintsFromAssumptions

      (other, otherMap) <- mkConstraints e rest
      pure (result ++ other, Map.insert sqlVar v otherMap)

data InferError =
  IAnnotateError AnnotateErr
  | IUnificationError [Constraint] (UnificationError TyVar)
  deriving (Eq, Show)

instance Pretty InferError where
  pretty = \case
    IAnnotateError e         -> viaShow e
    IUnificationError cons e -> hang 2 $ vsep [pretty cons, pretty e]

{-| Infer the type of a scalar expression under the given type env
-}
runInferType :: TypeEnv -> ScalarExpr -> Either InferError (Substitution TyVar, Tp TyVar, Map.Map SqlVar (Tp TyVar))
runInferType env = runIdentity . runExceptT . evalFreshT . inferType env

{-| Infer the type of a scalar expression
-}
inferType :: (MonadError InferError m, MonadFresh m) => TypeEnv -> ScalarExpr -> m (Substitution TyVar, Tp TyVar, Map.Map SqlVar (Tp TyVar))
inferType typeEnv expr = do
  (tp, (assumptions, constraints)) <- runExceptT (runAnnotateT (cataA typeVars expr)) >>= either (throwError . IAnnotateError) pure
  (constraints', assignments) <- mkConstraints typeEnv assumptions
  let allConstraints = constraints <> constraints'
  subs <- runExceptT (solve allConstraints) >>= either (throwError . IUnificationError allConstraints) pure
  return (subs, apply subs tp, fmap (apply subs) assignments)

data UnificationError v =
  UnificationFailed (Tp v) (Tp v)
  | ExpectedVar (Tp v)
  | Mgu (Tp v) (Tp v) (UnificationError v)
  | MguRow (UnificationError v)
  | MguSql (UnificationError v)
  | MguArr (Tp v, Tp v) (Tp v, Tp v) (UnificationError v)
  | MguGoal (TyConstraint TyVar (Tp TyVar)) (UnificationError v)
  | MguInstantiate (TyScheme v (Tp v)) (Tp TyVar) (UnificationError v)
  deriving (Eq, Show)

getFailure :: UnificationError v -> Maybe (Tp v, Tp v)
getFailure = \case
  UnificationFailed a b -> Just (a, b)
  ExpectedVar _         -> Nothing
  Mgu _ _ e             -> getFailure e
  MguRow e              -> getFailure e
  MguSql e              -> getFailure e
  MguArr _ _ e          -> getFailure e
  MguGoal _ e           -> getFailure e
  MguInstantiate _ _ e  -> getFailure e

instance Pretty v => Pretty (UnificationError v) where
  pretty = \case
    UnificationFailed a b -> "ERROR: Cannot unify" <+> pretty a <+> "with" <+> pretty b
    ExpectedVar v -> "ERROR: Expected a type variable, but got" <+> pretty v
    Mgu a b e -> vsep ["MGU" <+> pretty a <+> "with" <+> pretty b, pretty e]
    MguRow e -> vsep ["MGU Row", pretty e]
    MguSql e -> vsep ["MGU Sql", pretty e]
    MguInstantiate a b e -> vsep
      [ "Instantiating" <+> pretty a <+> "to" <+> pretty b
      , pretty e
      ]
    MguArr a b e ->
      let k (x, y) = pretty x <+> "->" <+> pretty y
      in vsep ["MGU Arrow" <+> k a <+> "with" <+> k b, pretty e]
    MguGoal c e -> hang 2 $ vsep ["Goal" <> colon <+> pretty c, pretty e]

wrapError :: forall m e e' a. MonadError e m => (e' -> e) -> ExceptT e' m a -> m a
wrapError f action = runExceptT action >>= \case
  Left err -> throwError (f err)
  Right a  -> pure a

{-| most general unifier of two types. If @mgu a b == Just s@ then it
should hold that @apply s a == apply s b@.

See Samuel R. Buss: Chapter I - An Introduction to Proof Theory in Handbook of Proof Theory (Elsevier 1998)
-}
mgu :: forall m. (MonadFresh m, MonadError (UnificationError TyVar) m) => Tp TyVar -> Tp TyVar -> m (Substitution TyVar)
mgu a b = wrapError (Mgu a b) $ go (a, b) where
  go :: forall m'. (MonadFresh m', MonadError (UnificationError TyVar) m') => (Tp TyVar, Tp TyVar) -> m' (Substitution TyVar)
  go = \case
    (TpVar a', TpVar b') | a' == b' -> pure mempty
    (TpVar a', term)
      | a' `elem` freeVars term -> throwError (UnificationFailed (TpVar a') term)
      | otherwise -> pure (singleton a' term)
    (term, TpVar a')
      | a' `elem` freeVars term -> throwError (UnificationFailed term (TpVar a'))
      | otherwise -> pure (singleton a' term)
    (TpSql a', TpSql b') -> mguT a' b'
    (TpRow a', TpRow b') -> mguR a' b'
    (TpArr a' b', TpArr c' d') -> do
      subs' <- go (a', c')
      k <- wrapError (MguArr (a', b') (c', d')) (go (apply subs' b', apply subs' d'))
      -- pure (subs' `comp` k)
      pure (subs' <> k)
    _ -> throwError $ UnificationFailed a b

mguR :: forall m. (MonadFresh m, MonadError (UnificationError TyVar) m) => RowType TyVar -> RowType TyVar -> m (Substitution TyVar)
mguR (RowType (TpVar v1) m1) (RowType (TpVar v2) m2) = wrapError MguRow $ do
  v' <- freshVar
  -- (RowType, substitution, RowType)
  (RowType (TpVar v') -> left, middle, RowType (TpVar v') -> right) <- flip execStateT (Map.empty, mempty, Map.empty) $ Merge.mergeA
    (Merge.traverseMissing $ \k v -> modifying _1 (Map.insert k v))
    (Merge.traverseMissing $ \k v -> modifying _3 (Map.insert k v))
    (Merge.zipWithAMatched $ \_ v1' v2' -> use _2 >>= \subs -> lift (mgu (apply subs v1') (apply subs v2')) >>= assign _2)
    m1 m2
  pure
    $ insertRow v2 left
    $ insertRow v1 right
    $ middle
mguR (RowType v1 _) (RowType (TpVar _) _) = throwError (ExpectedVar v1)
mguR _ (RowType v2 _) = throwError (ExpectedVar v2)

mguT :: forall m v. (MonadError (UnificationError v) m, Ord v) => SqlType -> SqlType -> m (Substitution v)
mguT a b = wrapError MguSql $ go (a, b) where
  go = \case
    (x, y) | x == y    -> pure mempty
           | otherwise -> throwError (UnificationFailed (TpSql x) (TpSql y))

{-| Function type with a list of arguments and a result
-}
arr :: [Tp a] -> Tp a -> Tp a
arr []     r = r
arr (x:xs) r = TpArr x (arr xs r)

data AnnotateErr =
  UnsupportedScalarExpr String
  | EmptyIdentifier
  deriving (Eq, Ord, Show)

type Annotate = AnnotateT (FreshT (ExceptT AnnotateErr Identity))

runAnnotate :: Annotate a -> Either AnnotateErr (a, ([Assumption], [Constraint]))
runAnnotate = runIdentity . runExceptT . evalFreshT . runAnnotateT

{-| Generate a fresh type variable for a term-level variable
-}
bindVar :: (MonadAnnotate m, MonadFresh m) => SqlVar -> m (Tp TyVar)
bindVar v = do
  tv <- TpVar <$> freshVar
  write ([(v, tv)], [])
  pure tv

{-| Emit constraints for an operator of the given name
-}
operator :: (MonadFresh m, MonadAnnotate m) =>
  [Name]
  -- ^ Name of the operator
  -> [Tp TyVar]
  -- ^ Types of the operator's arguments
  -> m (Tp TyVar)
  -- ^ Type variable for the operator type
operator names tps = do
  result <- TpVar <$> freshVar
  write ([(AnOperator names, arr tps result)], [])
  -- opType <- TpVar <$> freshVar
  -- write ([(AnOperator names, opType)], [TyEq opType (arr tps result)])
  pure result

identifier :: (MonadError AnnotateErr m, MonadFresh m, MonadAnnotate m) =>
  [Name]
  -> m (Tp TyVar)
identifier []  = throwError EmptyIdentifier
identifier [x] = bindVar (AnIdentifier [x])
identifier xs = do
  x <- mkAt [] xs
  tv <- TpVar <$> freshVar
  write ([(AnIdentifier xs, tv)], [TyEq tv x])
  pure x

{-| Accessing a record
-}
mkAt :: (MonadError AnnotateErr m, MonadFresh m, MonadAnnotate m) => [Name] -> [Name] -> m (Tp TyVar)
mkAt ys = \case
  [] -> throwError EmptyIdentifier
  [_x] -> TpVar <$> freshVar
  -- c . y
  (x : y : zs) -> do
    fieldTp <- mkAt (x:ys) (y:zs)
    row0 <- freshVar
    k <- TpVar <$> freshVar
    let recType = TpRow $ mkRow row0 [(y, fieldTp)]
    write ([(AnIdentifier (x:ys), k)], [TyEq k recType])
    pure fieldTp


{-| Emit constraints declaring the provided types equal
-}
allEquals :: MonadAnnotate m => [Tp TyVar] -> m ()
allEquals tps =
  let cons = zipWith TyEq tps (drop 1 tps)
  in write ([], cons)

typeConstraints :: ScalarExpr -> Either AnnotateErr (Tp TyVar, ([Assumption], [Constraint]))
typeConstraints = runAnnotate . cataA typeVars

{-| Produce the type variables and assumptions for a scalar expression.
-}
typeVars :: (MonadError AnnotateErr m, MonadAnnotate m, MonadFresh m) => ScalarExprF (m (Tp TyVar)) -> m (Tp TyVar)
typeVars = \case
  NumLit{}                 -> pure (TpSql STNumber)
  StringLit{}              -> pure (TpSql STText)
  IntervalLit{}            -> pure (TpSql STInterval)
  TypedLit tn _            -> pure (TpSql $ STOtherSqlType tn)
  Iden names               -> identifier names -- bindVar (AnIdentifier names)
  PositionalArg i          -> bindVar (APosArg i)
  HostParameter a b        -> bindVar (AHostParameter a b)
  BinOp x1 opNames x2      -> sequence [x1, x2] >>= operator opNames
  PrefixOp opNames x       -> x >>= operator opNames . return
  PostfixOp opNames x      -> x >>= operator opNames . return
  SpecialOp names args     -> sequence args >>= operator names
  App names args           -> sequence args >>= operator names
  Parens x                 -> x
  Cast x (TpSql . STOtherSqlType -> tn) ->
     -- TODO: do we need to check that the cast is legit? (This probably depends on the SQL dialect)
    x >> pure tn
  In _ b (InList bs)       -> sequence (b:bs) >>= allEquals >> pure (TpSql STBool)
  k                        -> throwError $ UnsupportedScalarExpr (show $ void k)
