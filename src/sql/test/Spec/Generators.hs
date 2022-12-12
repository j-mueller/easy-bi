{-# LANGUAGE LambdaCase #-}
{-| QuickCheck generators
-}
module Spec.Generators(
  sqlTypePrim,
  name,

  -- * Modifying sql types
  generalise,
  modify
  ) where

import           Control.Monad.Trans.Class     (MonadTrans (..))
import           Data.Maybe                    (fromMaybe)
import           EasyBI.Sql.Effects.Fresh      (evalFreshT, freshVar)
import           EasyBI.Sql.Effects.Types      (SqlType (..), TyVar)
import           Language.SQL.SimpleSQL.Syntax (IntervalTypeField (..),
                                                Name (..), PrecMultiplier (..),
                                                PrecUnits (..), TypeName (..))
import           Test.Tasty.QuickCheck         (Arbitrary (..), Gen,
                                                Positive (..),
                                                arbitraryPrintableChar,
                                                elements, listOf, listOf1,
                                                oneof, scale, suchThatMaybe)

sqlTypePrim :: Gen (SqlType v)
sqlTypePrim = do
  oneof
    [ elements [STNumber, STText, STBool, STInterval]
    , fmap STSqlType typeName
    , STArr <$> sqlTypePrim <*> sqlTypePrim
    ]

{-| Replace some types with fresh type variables
-}
generalise :: SqlType TyVar -> Gen (SqlType TyVar)
generalise = evalFreshT . go where
  f k = freshVar >>= \v -> lift (elements [k, STVar v])
  go = \case
    STVar v -> pure (STVar v)
    arr@(STArr a b) -> do
      arr' <- STVar <$> freshVar
      a' <- go a
      b' <- go b
      lift (elements [arr', STArr a' b, STArr a b', arr])
    k -> f k

{-| Change the type so that it can't be unified anymore
-}
modify :: SqlType TyVar -> Gen (Maybe (SqlType TyVar))
modify tp = suchThatMaybe (go tp) (/= tp) where
  go = \case
    STVar v -> pure (STVar v)
    STArr a b ->
      oneof
        [ pure (STArr a b)
        , STArr <$> fmap (fromMaybe a) (modify a) <*> fmap (fromMaybe b) (modify b)
        , sqlTypePrim
        ]
    x -> oneof [pure x, sqlTypePrim]

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
