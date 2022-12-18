{-# LANGUAGE LambdaCase #-}
{-| QuickCheck generators
-}
module Spec.Generators(
  sqlTypePrim,
  name,
  tp,

  -- * Modifying sql types
  generalise,
  modify
  ) where

import           Control.Monad.Trans.Class     (MonadTrans (..))
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as Map
import           EasyBI.Sql.Effects.Fresh      (FreshT, MonadFresh, evalFresh,
                                                evalFreshT, freshVar)
import           EasyBI.Sql.Effects.Types      (RowType (..), SqlType (..),
                                                Tp (..), TyVar)
import           Language.SQL.SimpleSQL.Syntax (IntervalTypeField (..),
                                                Name (..), PrecMultiplier (..),
                                                PrecUnits (..), TypeName (..))
import           Test.Tasty.QuickCheck         (Arbitrary (..), Gen,
                                                Positive (..),
                                                arbitraryPrintableChar,
                                                elements, listOf, listOf1,
                                                oneof, scale, suchThatMaybe)

sqlTypePrim :: Gen SqlType
sqlTypePrim = do
  oneof
    [ elements [STNumber, STText, STBool, STInterval]
    , fmap STSqlType typeName
    ]

{-| A type
-}
tp :: Gen (Tp TyVar)
tp = do
  k <- tpF
  pure (evalFresh $ freshRowVar k)

{-| Replace all row type variables with fresh ones
-}
freshRowVar :: MonadFresh m => Tp TyVar -> m (Tp TyVar)
freshRowVar = \case
  TpArr l r -> TpArr <$> freshRowVar l <*> freshRowVar r
  TpRow (RowType _a mp) -> fmap TpRow (RowType <$> fmap TpVar freshVar <*> traverse freshRowVar mp)
  x -> pure x

tpF :: Gen (Tp TyVar)
tpF = do
  oneof
    [ fmap TpSql sqlTypePrim
    , TpArr <$> tp <*> tp
    , fmap TpRow (RowType (TpVar 0) <$> genRow)
    ]

genRow :: Gen (Map Name (Tp TyVar))
genRow =
  let elm = (,) <$> name <*> scale (`div` 2) tp
  in Map.fromList <$> listOf elm

generalise :: Tp TyVar -> Gen (Tp TyVar)
generalise = evalFreshT . go where
  go :: Tp TyVar -> FreshT Gen (Tp TyVar)
  go = \case
    TpSql t -> do
      f <- TpVar <$> freshVar
      lift $ elements [TpSql t, f]
    TpRow r -> TpRow <$> generaliseRow r
    TpArr l r -> do
      l' <- go l
      r' <- go r
      lift $ elements [TpArr l' r, TpArr l r', TpArr l' r']
    x -> pure x

generaliseRow :: RowType TyVar -> FreshT Gen (RowType TyVar)
generaliseRow r@(RowType a mp) = pure r -- fixme

{-| Change the type so that it can't be unified anymore
-}
modify :: Tp TyVar -> Gen (Maybe (Tp TyVar))
modify tp' = suchThatMaybe (go tp') (/= tp') where
  go = \case
    TpSql t              -> oneof [pure (TpSql t), TpSql <$> sqlTypePrim]
    TpRow (RowType a mp) -> TpRow . RowType a <$> traverse go mp
    TpArr l r            -> TpArr <$> go l <*> go r
    TpVar _v             -> TpSql <$> sqlTypePrim

typeName :: Gen TypeName
typeName =
  let posInteger = getPositive <$> arbitrary
      mb g = oneof [pure Nothing, Just <$> g]
  in oneof
      [ TypeName <$> listOf1 name
      , PrecTypeName <$> listOf1 name <*> posInteger
      , PrecScaleTypeName <$> listOf1 name <*> posInteger <*> posInteger
      , PrecLengthTypeName <$> listOf1 name <*> posInteger <*> mb precMultiplier <*> mb precUnits
      , CharTypeName <$> listOf1 name <*> mb posInteger <*> listOf name <*> listOf name
      , TimeTypeName <$> listOf1 name <*> mb posInteger <*> elements [True, False]
      , let gn = (,) <$> name <*> scale (`div` 2) typeName
        in RowTypeName <$> listOf gn
      , IntervalTypeName <$> intervalTypeField <*> mb intervalTypeField
      , ArrayTypeName <$> (scale (`div` 2) typeName) <*> mb posInteger
      , MultisetTypeName <$> scale (`div` 2) typeName
      ]

name :: Gen Name
name =
  let quotes = elements [Nothing, Just ("'", "'"), Just ("[", "]"), Just ("\"", "\""), Just ("`", "`")]
  in Name <$> quotes <*> listOf arbitraryPrintableChar

precMultiplier :: Gen PrecMultiplier
precMultiplier = elements [PrecK, PrecM, PrecG, PrecT, PrecP]

precUnits :: Gen PrecUnits
precUnits = elements [PrecCharacters, PrecOctets]

intervalTypeField :: Gen IntervalTypeField
intervalTypeField = Itf <$> listOf arbitraryPrintableChar <*> pure Nothing
