{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-| Visualisations
-}
module EasyBI.Server.Visualisation
  ( APIQuery (..)
  , Dimension (..)
  , Field (..)
  , Filter (..)
  , Measure (..)
  , SortOrder (..)
  , SqlFieldName (..)
  , TimeGranularity (..)
  , Visualisation (..)
  , _dimensionDisplayName
  , _displayName
  , _measureDisplayName
  , fields
  , fromSqlName
  , sqlFieldName
  , toSqlName
  , visualisations
  ) where

import Codec.Serialise               (Serialise (..))
import Control.Lens                  (Lens', lens, over)
import Control.Lens                  qualified as L
import Data.Aeson                    (FromJSON (..), ToJSON (..), object, (.=))
import Data.Aeson                    qualified as JSON
import Data.Aeson.KeyMap             qualified as KM
import Data.Bifunctor                (Bifunctor (..))
import Data.Containers.ListUtils     (nubOrd)
import Data.List                     (nub, sortOn)
import Data.Map                      (Map)
import Data.Map                      qualified as Map
import Data.Maybe                    (mapMaybe)
import Data.Ord                      (Down (..))
import Data.String                   (IsString (..))
import Data.Text                     qualified as Text
import EasyBI.Sql.Effects.Types      (SqlType (..), Tp (..), TyVar)
import EasyBI.Util.JSON              (WrappedObject (..), _WrappedObject,
                                      customJsonOptions, fromValue)
import EasyBI.Util.NiceHash          (HasNiceHash, NiceHashable (..))
import EasyBI.Vis.Charts             (Chart)
import EasyBI.Vis.HVega              qualified as HVega
import EasyBI.Vis.Rules              (makeChart)
import EasyBI.Vis.Types              (Archetype, Measurement (..),
                                      Relation (..), Selections, archetype,
                                      initialSelections, runRule)
import GHC.Generics                  (Generic)
import Language.SQL.SimpleSQL.Syntax qualified as Syntax

newtype Score = Score Int
  deriving stock (Generic, Show)
  deriving newtype (ToJSON, FromJSON, Serialise, Ord, Eq)

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
    , visApiQuery    :: APIQuery
    -- ^ The fields used by this visualisation
    , visEncoding    :: Chart Field
    , visQuery       :: a
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON, Serialise)

visualisations :: a -> Selections [] (Field) -> [Visualisation a]
visualisations hsh selections =
  let usesAllFields Visualisation{visApiQuery} = length (apiQueryFields visApiQuery) == length selections
      addScore x = traverse score (x, x)
      score _ = pure (Score 10)
      mkSel = take 10 . filter usesAllFields . mapMaybe (uncurry (enc hsh)) . sortOn (Down . snd) . mapMaybe addScore . nubOrd . runRule 50 makeChart
  in mconcat (mkSel <$> initialSelections selections)

{-| The fields of a record
-}
fields :: Map Syntax.Name (Tp TyVar) -> [Field]
fields mp = mapMaybe (fmap (uncurry mkField . first getName) . traverse getMeasure) (Map.toList mp) where
  getName (Syntax.Name _ n) = SqlFieldName n
  getMeasure (TpSql t) = case t of
    STNumber   -> Just Quantitative
    STInt      -> Just Ordinal -- TODO: Could be quant. sometimes?
    STText     -> Just Nominal
    STBool     -> Just Nominal
    STDateTime -> Just TemporalAbs
    STTemporal -> Just TemporalRel
    _          -> Nothing
  getMeasure _ = Nothing

  mkField :: SqlFieldName -> Measurement -> Field
  mkField n m =
    let displayName = Just $ Text.pack $ getSqlFieldName n in
    case m of
    Quantitative ->
      AMeasure $ Measure n displayName
    TemporalAbs ->
      ADimension $ Dimension n displayName m Ascending
    _ ->
      ADimension $ Dimension n displayName m None

enc :: a -> Chart (Field) -> Score -> Maybe (Visualisation a)
enc hsh chart score_ =
  let setData = KM.insert "data" (object ["name" .= s "table"])
                . KM.insert "width" (toJSON (s "container"))
                . KM.insert "height" (toJSON (s "container"))
  in Visualisation
      <$> fmap (over _WrappedObject setData) (fromValue (HVega.toJSON chart))
      <*> pure "FIXME: enc.visDescription"
      <*> pure score_
      <*> pure (archetype chart)
      <*> pure (foldMap queryFromField chart)
      <*> pure chart
      <*> pure hsh

data SortOrder = Ascending | Descending | None
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

data TimeGranularity = ByDay | ByMonth | ByYear | ByQuarter
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

{-| Filter applied to a field
-}
data Filter
  = TopN{n :: Int} -- TODO: Include measure (currently it's just COUNT)
  | IncludeList{ include :: [String]}
  | NoFilter
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

{-| A dimension with a type
-}
data Dimension =
  Dimension
    { dimensionSqlFieldName    :: SqlFieldName
    , dimensionDisplayName     :: Maybe Text.Text
    , dimensionType            :: Measurement
    , dimensionDefaultSortOder :: SortOrder
    }
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Serialise)
    deriving HasNiceHash via (NiceHashable "dimension" Dimension)

instance ToJSON Dimension where
  toJSON = JSON.genericToJSON (customJsonOptions 9)
  toEncoding = JSON.genericToEncoding (customJsonOptions 9)

instance FromJSON Dimension where
  parseJSON = JSON.genericParseJSON (customJsonOptions 9)

_dimensionDisplayName :: Lens' Dimension (Maybe Text.Text)
_dimensionDisplayName = lens get set where
  get :: Dimension -> Maybe Text.Text
  get = dimensionDisplayName
  set :: Dimension -> Maybe Text.Text -> Dimension
  set x t = x { dimensionDisplayName = t}

data Measure =
  Measure
    { measureSqlFieldName :: SqlFieldName
    , measureDisplayName  :: Maybe Text.Text
    }
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Serialise)
    deriving HasNiceHash via (NiceHashable "measure" Measure)

instance ToJSON Measure where
  toJSON = JSON.genericToJSON (customJsonOptions 7)
  toEncoding = JSON.genericToEncoding (customJsonOptions 7)

instance FromJSON Measure where
  parseJSON = JSON.genericParseJSON (customJsonOptions 7)

_measureDisplayName :: Lens' Measure (Maybe Text.Text)
_measureDisplayName = lens get set where
  get = measureDisplayName
  set x t = x { measureDisplayName = t}

_displayName :: Lens' Field (Maybe Text.Text)
_displayName = lens get s_ where
  get (ADimension d) = L.view _dimensionDisplayName d
  get (AMeasure m)   = L.view _measureDisplayName m

  s_ (ADimension d) t = ADimension $ L.set _dimensionDisplayName t d
  s_ (AMeasure m)   t = AMeasure $ L.set _measureDisplayName t m

data Field =
  ADimension Dimension
  | AMeasure Measure
    deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON, Serialise)
    deriving HasNiceHash via (NiceHashable "field" Field)

sqlFieldName :: Field -> SqlFieldName
sqlFieldName = \case
  ADimension Dimension{dimensionSqlFieldName} -> dimensionSqlFieldName
  AMeasure Measure{measureSqlFieldName}       -> measureSqlFieldName

instance Relation Field where
  measurement = \case
    ADimension Dimension{dimensionType} -> dimensionType
    AMeasure{}                          -> Quantitative
  fieldName (AMeasure Measure{measureSqlFieldName}) = Text.pack $ getSqlFieldName measureSqlFieldName
  fieldName (ADimension Dimension{dimensionSqlFieldName}) = Text.pack $ getSqlFieldName dimensionSqlFieldName
  fieldLabel k = maybe (fieldName k) id (L.view _displayName k)

s :: String -> String
s = id

-- | The name of a field in SQL
newtype SqlFieldName = SqlFieldName{ getSqlFieldName :: String }
  deriving newtype (Eq, Ord, IsString, ToJSON, FromJSON, Serialise)
  deriving stock Show

-- FIXME: Maybe this should just be a newtype wrapper for Syntax.Name
-- or do we need CanonicalName? (ignoring the quote marks)

fromSqlName :: Syntax.Name -> SqlFieldName
fromSqlName (Syntax.Name _ n) = SqlFieldName n

toSqlName :: SqlFieldName -> Syntax.Name
toSqlName (SqlFieldName n) = Syntax.Name Nothing n

data APIQuery =
  APIQuery
    { apiQuerySplits    :: [SqlFieldName]
    , apiQueryMeasures  :: [SqlFieldName]
    , apiQueryFilters   :: [(SqlFieldName, Filter)]
    , apiQuerySortOrder :: [(SqlFieldName, SortOrder)]
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (Serialise)

instance ToJSON APIQuery where
  toJSON = JSON.genericToJSON (customJsonOptions 8)
  toEncoding = JSON.genericToEncoding (customJsonOptions 8)

instance FromJSON APIQuery where
  parseJSON = JSON.genericParseJSON (customJsonOptions 8)

instance Semigroup APIQuery where
  l <> r =
    APIQuery
      { apiQuerySplits   = apiQuerySplits l <> apiQuerySplits r
      , apiQueryMeasures = apiQueryMeasures l <> apiQueryMeasures r
      , apiQueryFilters = apiQueryFilters l <> apiQueryFilters r
      , apiQuerySortOrder = apiQuerySortOrder l <> apiQuerySortOrder r
      }

instance Monoid APIQuery where
  mempty = APIQuery mempty mempty mempty mempty

-- | An @APIQuery@ object with the field as split or measure, depending on the field's type.
-- Adds default sort order and filter.
queryFromField :: Field -> APIQuery
queryFromField = \case
  ADimension dim ->
    let fieldName = dimensionSqlFieldName dim
        filters =
          case dimensionType dim of
            Nominal -> [(fieldName, TopN 15)]
            Ordinal -> [(fieldName, TopN 15)]
            _       -> []
    in mempty{apiQuerySplits = [fieldName], apiQueryFilters = filters}
  AMeasure mes   ->
    mempty{apiQueryMeasures=[measureSqlFieldName mes]}

apiQueryFields :: APIQuery -> [SqlFieldName]
apiQueryFields APIQuery{apiQuerySplits, apiQueryMeasures, apiQueryFilters, apiQuerySortOrder} =
  nub (apiQuerySplits ++ apiQueryMeasures ++ fmap fst apiQueryFilters ++ fmap fst apiQuerySortOrder)

