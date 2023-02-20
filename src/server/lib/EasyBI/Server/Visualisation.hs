{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeFamilies       #-}
{-| Visualisations
-}
module EasyBI.Server.Visualisation
  ( Field (..)
  , fields
  , Visualisation (..)
  , visualisations
  ) where

import           Codec.Serialise               (Serialise (..))
import           Control.Lens                  (over, view, (&), (.~))
import           Data.Aeson                    (FromJSON (..), ToJSON (..),
                                                object, (.=))
import qualified Data.Aeson.KeyMap             as KM
import           Data.Bifunctor                (Bifunctor (..))
import           Data.Foldable                 (toList)
import           Data.List                     (sortOn)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe, mapMaybe)
import           Data.Ord                      (Down (..))
import qualified Data.Text                     as Text
import           EasyBI.Sql.Catalog            (TypedQueryExpr)
import           EasyBI.Sql.Effects.Types      (RowType (..), SqlType (..),
                                                Tp (..), TyScheme (..), TyVar)
import           EasyBI.Util.JSON              (WrappedObject (..),
                                                _WrappedObject, fromValue)
import           EasyBI.Util.NiceHash          (HasNiceHash (..), NiceHash)
import qualified EasyBI.Vis.HVega              as HVega
import           EasyBI.Vis.Rules              (makeChart)
import           EasyBI.Vis.Types              (Archetype (Misc), Encoding,
                                                Measurement (..), Relation (..),
                                                Score (..), Selections,
                                                archetype, emptySelections,
                                                runRule, score, wildCards)
import           GHC.Generics                  (Generic)
import qualified Language.SQL.SimpleSQL.Syntax as Syntax

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
    , visFieldNames  :: [String]
    -- ^ The fields used by this visualisation
    , visEncoding    :: Encoding Field
    , visQuery       :: a
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON, Serialise)

instance HasNiceHash (Visualisation (NiceHash TypedQueryExpr)) where
  type Name (Visualisation (NiceHash TypedQueryExpr)) = "vis"

visualisations :: a -> TyScheme TyVar (Tp TyVar) -> [Visualisation a]
visualisations hsh =
  let addScore x = traverse score (x, x) in
  maybe []
    (take 10
      . mapMaybe (uncurry (enc hsh))
      . sortOn (Down . snd)
      . mapMaybe addScore
      . runRule 50 makeChart)
    . selections

selections :: TyScheme TyVar (Tp TyVar) -> Maybe (Selections Field)
selections (TyScheme _ (TpRow (RowType _ mp))) = Just (emptySelections & wildCards .~ fields mp)
selections _                                   = Nothing

{-| The fields of a record
-}
fields :: Map Syntax.Name (Tp TyVar) -> [Field]
fields mp = mapMaybe (fmap (uncurry Field . first getName) . traverse getMeasure) (Map.toList mp) where
  getName (Syntax.Name _ n) = n
  getMeasure (TpSql t) = case t of
    STNumber   -> Just Quantitative
    STInt      -> Just Ordinal -- TODO: Could be quant. sometimes?
    STText     -> Just Nominal
    STBool     -> Just Nominal
    STDateTime -> Just TemporalAbs
    _          -> Nothing
  getMeasure _ = Nothing

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
      <*> pure (name <$> toList e)
      <*> pure e
      <*> pure hsh

{-| A field with a measurement
-}
data Field = Field{ name :: String, fieldType :: Measurement }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

instance Relation Field where
  measurement = fieldType
  fieldName = Text.pack . name

s :: String -> String
s = id
