{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
{-| Visualisations
-}
module EasyBI.Server.Visualisation
  ( Field (..)
  , SortOrder (..)
  , Visualisation (..)
  , fields
  , visualisations
  ) where

import Codec.Serialise               (Serialise (..))
import Control.Lens                  (over, view)
import Data.Aeson                    (FromJSON (..), ToJSON (..), object, (.=))
import Data.Aeson.KeyMap             qualified as KM
import Data.Bifunctor                (Bifunctor (..))
import Data.Containers.ListUtils     (nubOrd)
import Data.Foldable                 (toList)
import Data.List                     (sortOn)
import Data.Map                      (Map)
import Data.Map                      qualified as Map
import Data.Maybe                    (fromMaybe, mapMaybe)
import Data.Ord                      (Down (..))
import Data.Text                     qualified as Text
import EasyBI.Sql.Catalog            (TypedQueryExpr)
import EasyBI.Sql.Effects.Types      (SqlType (..), Tp (..), TyVar)
import EasyBI.Util.JSON              (WrappedObject (..), _WrappedObject,
                                      fromValue)
import EasyBI.Util.NiceHash          (HasNiceHash (..), NiceHash)
import EasyBI.Vis.HVega              qualified as HVega
import EasyBI.Vis.Rules              (makeChart)
import EasyBI.Vis.Types              (Archetype (Misc), Encoding,
                                      Measurement (..), Relation (..),
                                      Score (..), Selections, archetype,
                                      initialSelections, runRule, score)
import GHC.Generics                  (Generic)
import Language.SQL.SimpleSQL.Syntax qualified as Syntax

{-| Visualisation to be shown on the client
-}
data Visualisation a =
  Visualisation
    { visDefinition  :: WrappedObject
    -- ^ Specification of the graph in HVega
    , visDescription :: String
    -- ^ Description
    , visScore       :: Score
    -- ^ Score
    , visArchetype   :: Archetype
    -- ^ Archetype of the visualisation
    , visFields      :: [Field]
    -- ^ The fields used by this visualisation
    , visEncoding    :: Encoding Field
    , visQuery       :: a
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON, Serialise)

instance HasNiceHash (Visualisation (NiceHash TypedQueryExpr)) where
  type Name (Visualisation (NiceHash TypedQueryExpr)) = "vis"

visualisations :: a -> Selections [] Field -> [Visualisation a]
visualisations hsh selections =
  let addScore x = traverse score (x, x)
      mkSel = take 10 . mapMaybe (uncurry (enc hsh)) . sortOn (Down . snd) . mapMaybe addScore . nubOrd . runRule 50 makeChart
  in mconcat (mkSel <$> initialSelections selections)

{-| The fields of a record
-}
fields :: Map Syntax.Name (Tp TyVar) -> [Field]
fields mp = mapMaybe (fmap (uncurry mkField . first getName) . traverse getMeasure) (Map.toList mp) where
  getName (Syntax.Name _ n) = n
  getMeasure (TpSql t) = case t of
    STNumber   -> Just Quantitative
    STInt      -> Just Ordinal -- TODO: Could be quant. sometimes?
    STText     -> Just Nominal
    STBool     -> Just Nominal
    STDateTime -> Just TemporalAbs
    _          -> Nothing
  getMeasure _ = Nothing
  mkField name fieldType = Field{name, fieldType, sortOrder = None}

enc :: a -> Encoding Field -> Score -> Maybe (Visualisation a)
enc hsh e score_ =
  let setData = KM.insert "data" (object ["name" .= s "table"])
                . KM.insert "width" (toJSON (s "container"))
                . KM.insert "height" (toJSON (s "container"))
  in Visualisation
      <$> fmap (over _WrappedObject setData) (fromValue (HVega.toJSON e))
      <*> pure "FIXME: enc.visDescription"
      <*> pure score_
      <*> pure (fromMaybe Misc (view archetype e))
      <*> pure (toList e)
      <*> pure e
      <*> pure hsh

data SortOrder = Ascending | Descending | None
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

{-| A field with a measurement
-}
data Field =
  Field
    { name      :: String
    , fieldType :: Measurement
    , sortOrder :: SortOrder
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

instance Relation Field where
  measurement = fieldType
  fieldName = Text.pack . name

s :: String -> String
s = id
