{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-| Visualisations
-}
module EasyBI.Server.Visualisation
  ( AMeasurement (..)
  , FieldInMode (..)
  , Filter (..)
  , InOut (..)
  , MeasurementAndField (..)
  , SomeMeasurement (..)
  , SortOrder (..)
  , SqlFieldName (..)
  , TimeGranularity (..)
  , Visualisation (..)
  , _displayName
  , _sortOrder
  , fields
  , fromMeasurement
  , fromSqlName
  , measurementVal
  , toSqlName
  , visualisations
  ) where

import Codec.Serialise               (Serialise (..))
import Control.Lens                  (Lens', lens, over, view)
import Data.Aeson                    (FromJSON (..), ToJSON (..), object,
                                      withText, (.=))
import Data.Aeson                    qualified as JSON
import Data.Aeson.KeyMap             qualified as KM
import Data.Aeson.Types              (Object, Parser, Value (String),
                                      unexpected, withObject, (.:))
import Data.Bifunctor                (Bifunctor (..))
import Data.Containers.ListUtils     (nubOrd)
import Data.Foldable                 (toList)
import Data.Functor                  (($>))
import Data.List                     (sortOn)
import Data.Map                      (Map)
import Data.Map                      qualified as Map
import Data.Maybe                    (fromMaybe, mapMaybe)
import Data.Ord                      (Down (..))
import Data.String                   (IsString (..))
import Data.Text                     qualified as Text
import EasyBI.Sql.Catalog            (TypedQueryExpr)
import EasyBI.Sql.Effects.Types      (SqlType (..), Tp (..), TyVar)
import EasyBI.Util.JSON              (SerialiseViaJSON (..), WrappedObject (..),
                                      _WrappedObject, customJsonOptions,
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
    , visFields      :: [FieldInMode In]
    -- ^ The fields used by this visualisation
    , visEncoding    :: Encoding (FieldInMode In)
    , visQuery       :: a
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON, Serialise)

instance HasNiceHash (Visualisation (NiceHash TypedQueryExpr)) where
  type Name (Visualisation (NiceHash TypedQueryExpr)) = "vis"

visualisations :: a -> Selections [] (FieldInMode In) -> [Visualisation a]
visualisations hsh selections =
  let addScore x = traverse score (x, x)
      mkSel = take 10 . mapMaybe (uncurry (enc hsh)) . sortOn (Down . snd) . mapMaybe addScore . nubOrd . runRule 50 makeChart
  in mconcat (mkSel <$> initialSelections selections)

{-| The fields of a record
-}
fields :: Map Syntax.Name (Tp TyVar) -> [FieldInMode Out]
fields mp = mapMaybe (fmap (uncurry mkField . first getName) . traverse getMeasure) (Map.toList mp) where
  getName (Syntax.Name _ n) = SqlFieldName n
  getMeasure (TpSql t) = case t of
    STNumber   -> Just (SomeMeasurement AQuantitativeMeasurement)
    STInt      -> Just (SomeMeasurement AnOrdinalMeasurement) -- TODO: Could be quant. sometimes?
    STText     -> Just (SomeMeasurement ANominalMeasurement)
    STBool     -> Just (SomeMeasurement ANominalMeasurement)
    STDateTime -> Just (SomeMeasurement ATemporalAbsMeasurement)
    STTemporal -> Just (SomeMeasurement ATemporalRelMeasurement)
    _          -> Nothing
  getMeasure _ = Nothing

  mkField :: SqlFieldName -> SomeMeasurement -> FieldInMode Out
  mkField n (SomeMeasurement mFieldType) =
    let displayName = Just $ Text.pack $ getSqlFieldName n in
    case mFieldType of
    ANominalMeasurement ->
      FieldInMode
        { sqlFieldName = n
        , displayName
        , fieldOptions = MeasurementAndField{mFieldType, mFilter=NoFilter}
        , sortOrder = None
        }
    AQuantitativeMeasurement ->
      FieldInMode
        { sqlFieldName = n
        , displayName
        , fieldOptions = MeasurementAndField{mFieldType, mFilter = NoFilter}
        , sortOrder = None
        }
    AnOrdinalMeasurement ->
      FieldInMode
        { sqlFieldName = n
        , displayName
        , fieldOptions = MeasurementAndField{mFieldType, mFilter = NoFilter}
        , sortOrder = Ascending
        }
    ATemporalAbsMeasurement ->
      FieldInMode
        { sqlFieldName = n
        , displayName
        , fieldOptions = MeasurementAndField{mFieldType, mFilter = NoFilter}
        , sortOrder = Ascending
        }
    ATemporalRelMeasurement ->
      FieldInMode
        { sqlFieldName = n
        , displayName
        , fieldOptions = MeasurementAndField{mFieldType, mFilter = NoFilter}
        , sortOrder = Ascending
        }

enc :: a -> Encoding (FieldInMode In) -> Score -> Maybe (Visualisation a)
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

-- | Whether we receive information (In) or send
--   information (out), from the server's perspective
data InOut = In | Out
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

data TimeGranularity = ByDay | ByMonth | ByYear | ByQuarter
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

data AMeasurement a where
  ANominalMeasurement :: AMeasurement Nominal
  AnOrdinalMeasurement :: AMeasurement Ordinal
  AQuantitativeMeasurement :: AMeasurement Quantitative
  ATemporalAbsMeasurement :: AMeasurement TemporalAbs
  ATemporalRelMeasurement :: AMeasurement TemporalRel

measurementTag :: AMeasurement a -> String
measurementTag = \case
  ANominalMeasurement      -> "nominal"
  AnOrdinalMeasurement     -> "ordinal"
  AQuantitativeMeasurement -> "quantitative"
  ATemporalAbsMeasurement  -> "temporal-abs"
  ATemporalRelMeasurement  -> "temporal-rel"

instance Show (AMeasurement a) where
  show = measurementTag

instance ToJSON (AMeasurement a) where
  toJSON = toJSON . measurementTag

data SomeMeasurement = forall a. SomeMeasurement (AMeasurement a)

-- | Value level measurement
measurementVal :: SomeMeasurement -> Measurement
measurementVal (SomeMeasurement a) = case a of
  ANominalMeasurement      -> Nominal
  AnOrdinalMeasurement     -> Ordinal
  AQuantitativeMeasurement -> Quantitative
  ATemporalAbsMeasurement  -> TemporalAbs
  ATemporalRelMeasurement  -> TemporalRel

deriving  via (SerialiseViaJSON SomeMeasurement) instance Serialise SomeMeasurement

instance ToJSON SomeMeasurement where
  toJSON (SomeMeasurement m) = toJSON m

instance FromJSON SomeMeasurement where
  parseJSON = withText "Measurement" $ \case
    "nominal"      -> pure (SomeMeasurement ANominalMeasurement)
    "ordinal"      -> pure (SomeMeasurement AnOrdinalMeasurement)
    "quantitative" -> pure (SomeMeasurement AQuantitativeMeasurement)
    "temporal-abs" -> pure (SomeMeasurement ATemporalAbsMeasurement)
    "temporal-rel" -> pure (SomeMeasurement ATemporalRelMeasurement)
    vl             -> unexpected (String vl)

toMeasurement :: SomeMeasurement -> Measurement
toMeasurement (SomeMeasurement a) = case a of
  ANominalMeasurement      -> Nominal
  AnOrdinalMeasurement     -> Ordinal
  AQuantitativeMeasurement -> Quantitative
  ATemporalAbsMeasurement  -> TemporalAbs
  ATemporalRelMeasurement  -> TemporalRel

fromMeasurement :: Measurement -> SomeMeasurement
fromMeasurement = \case
  Nominal      -> SomeMeasurement ANominalMeasurement
  Ordinal      -> SomeMeasurement AnOrdinalMeasurement
  Quantitative -> SomeMeasurement AQuantitativeMeasurement
  TemporalAbs  -> SomeMeasurement ATemporalAbsMeasurement
  TemporalRel  -> SomeMeasurement ATemporalRelMeasurement

data Filter mode (measure :: Measurement) where
  NominalList    :: [String] -> Filter Out Nominal
  NominalInclude :: [String] -> Filter In  Nominal
  NominalExclude :: [String] -> Filter In  Nominal
  NominalTopN    :: Int      -> Filter mode Nominal
  NoFilter :: Filter mode measure

instance ToJSON (Filter mode measurement) where
  toJSON = \case
    NominalList as    -> object ["tag" .= ("list" :: String), "values" .= as]
    NominalInclude as -> object ["tag" .= ("include" :: String), "values" .= as]
    NominalExclude as -> object ["tag" .= ("exclude" :: String), "values" .= as]
    NominalTopN n     -> object ["tag" .= ("top-n" :: String), "n" .= n]
    NoFilter          -> object ["tag" .= ("NoFilter" :: String)]

instance Show (Filter mode measure) where
  show = \case
    NominalList as    -> "NominalList " <> show as
    NominalInclude as -> "NominalInclude " <> show as
    NominalExclude as -> "NominalExclude "  <> show as
    NominalTopN n     -> "NominalTopN " <> show n
    NoFilter          -> "NoFilter"

instance Eq (Filter In Nominal) where
  l == r = case (l, r) of
    (NominalInclude as, NominalInclude bs) -> as == bs
    (NominalExclude as, NominalExclude bs) -> as == bs
    _                                      -> False

instance Eq (Filter In Ordinal) where
  l == r = case (l, r) of
    (NoFilter, NoFilter) -> True

instance Eq (Filter In Quantitative) where
  l == r = case (l, r) of
    (NoFilter, NoFilter) -> True

instance Eq (Filter In TemporalAbs) where
  l == r = case (l, r) of
    (NoFilter, NoFilter) -> True

instance Eq (Filter In TemporalRel) where
  l == r = case (l, r) of
    (NoFilter, NoFilter) -> True

instance Eq (Filter Out Nominal) where
  l == r = case (l, r) of
    (NominalList as, NominalList bs) -> as == bs
    (NominalTopN n, NominalTopN k)   -> n == k
    (NoFilter, NoFilter)             -> True
    (NoFilter, NominalList _)        -> False
    (NominalList _, NoFilter)        -> False
    (NoFilter, NominalTopN _)        -> False
    (NominalList _, NominalTopN _)   -> False
    (NominalTopN _, NoFilter)        -> False
    (NominalTopN _, NominalList _)   -> False

instance Eq (Filter Out Ordinal) where
  l == r = case (l, r) of
    (NoFilter, NoFilter) -> True

instance Eq (Filter Out Quantitative) where
  l == r = case (l, r) of
    (NoFilter, NoFilter) -> True

instance Eq (Filter Out TemporalAbs) where
  l == r = case (l, r) of
    (NoFilter, NoFilter) -> True

instance Eq (Filter Out TemporalRel) where
  l == r = case (l, r) of
    (NoFilter, NoFilter) -> True

instance FromJSON (Filter In Nominal) where
  parseJSON = withObject "Filter" $ \obj -> do
    (tg :: String) <- obj .: "tag"
    case tg of
      "include"  -> NominalInclude <$> obj .: "values"
      "exclude"  -> NominalExclude <$> obj .: "values"
      "top-n"    -> NominalTopN <$> obj .: "n"
      "NoFilter" -> pure NoFilter
      x          -> fail ("Unexpected tag: " <> x)

instance FromJSON (Filter In Ordinal) where
  parseJSON = withObject "Filter" (\obj -> expectTag obj "NoFilter" $> NoFilter)

instance FromJSON (Filter In Quantitative) where
  parseJSON = withObject "Filter" (\obj -> expectTag obj "NoFilter" $> NoFilter)

instance FromJSON (Filter In TemporalAbs) where
  parseJSON = withObject "Filter" (\obj -> expectTag obj "NoFilter" $> NoFilter)

instance FromJSON (Filter In TemporalRel) where
  parseJSON = withObject "Filter" (\obj -> expectTag obj "NoFilter" $> NoFilter)

instance FromJSON (Filter Out Nominal) where
  parseJSON = withObject "Filter" $ \obj ->
    expectTag obj "list" >> (NominalList <$> obj .: "values")

instance FromJSON (Filter Out Ordinal) where
  parseJSON = withObject "Filter" (\obj -> expectTag obj "NoFilter" $> NoFilter)

instance FromJSON (Filter Out Quantitative) where
  parseJSON = withObject "Filter" (\obj -> expectTag obj "NoFilter" $> NoFilter)

instance FromJSON (Filter Out TemporalAbs) where
  parseJSON = withObject "Filter" (\obj -> expectTag obj "NoFilter" $> NoFilter)

instance FromJSON (Filter Out TemporalRel) where
  parseJSON = withObject "Filter" (\obj -> expectTag obj "NoFilter" $> NoFilter)

expectTag :: Object -> Text.Text -> Parser ()
expectTag obj tg =
  obj .: "tag" >>= \case
      String k | k == tg -> pure ()
      String x           -> fail $ "Unexpected tag: " <> Text.unpack x <> ". Expected : " <> Text.unpack tg
      x -> fail $ "Unexpected value: " <> show x

{-| A field with a measurement
-}
data FieldInMode mode =
  FieldInMode
    { sqlFieldName :: SqlFieldName
    , displayName  :: Maybe Text.Text
    , fieldOptions :: MeasurementAndField mode
    , sortOrder    :: SortOrder
    }
    deriving Generic

_displayName :: Lens' (FieldInMode mode) (Maybe Text.Text)
_displayName = lens get set where
  get :: FieldInMode mode -> Maybe Text.Text
  get = displayName
  set :: FieldInMode mode -> Maybe Text.Text -> FieldInMode mode
  set x t = x { displayName = t}

_sortOrder :: Lens' (FieldInMode Out) SortOrder
_sortOrder = lens sortOrder (\x o -> x { sortOrder = o})

deriving instance Eq (FieldInMode In)
deriving instance Eq (FieldInMode Out)
deriving instance Ord (FieldInMode In)
deriving instance Ord (FieldInMode Out)

instance ToJSON (FieldInMode In) where
  toJSON = JSON.genericToJSON (customJsonOptions 0)
  toEncoding = JSON.genericToEncoding (customJsonOptions 0)

instance FromJSON (FieldInMode In) where
  parseJSON = withObject "FieldInMode In" $ \obj ->
    FieldInMode
      <$> obj .: "sql_field_name"
      <*> obj .: "display_name"
      <*> obj .: "field_options"
      <*> obj .: "sort_order"

instance ToJSON (FieldInMode Out) where
  toJSON = JSON.genericToJSON (customJsonOptions 0)
  toEncoding = JSON.genericToEncoding (customJsonOptions 0)

instance FromJSON (FieldInMode Out) where
  parseJSON = JSON.genericParseJSON (customJsonOptions 0)

data MeasurementAndField mode = forall measurement.
  MeasurementAndField
    { mFieldType :: AMeasurement measurement
    , mFilter    :: Filter mode measurement
    }

fieldType :: MeasurementAndField mode -> SomeMeasurement
fieldType MeasurementAndField{mFieldType} = SomeMeasurement mFieldType

instance Eq (MeasurementAndField In) where
  MeasurementAndField lType lGran == MeasurementAndField rType rGran =
    case (lType, rType) of
      (ANominalMeasurement, ANominalMeasurement)           -> lGran == rGran
      (AnOrdinalMeasurement, AnOrdinalMeasurement)         -> lGran == rGran
      (AQuantitativeMeasurement, AQuantitativeMeasurement) -> lGran == rGran
      (ATemporalAbsMeasurement, ATemporalAbsMeasurement)   -> lGran == rGran
      (ATemporalRelMeasurement, ATemporalRelMeasurement)   -> lGran == rGran
      _                                                    -> False

instance Eq (MeasurementAndField Out) where
  MeasurementAndField lType lGran == MeasurementAndField rType rGran =
    case (lType, rType) of
      (ANominalMeasurement, ANominalMeasurement)           -> lGran == rGran
      (AnOrdinalMeasurement, AnOrdinalMeasurement)         -> lGran == rGran
      (AQuantitativeMeasurement, AQuantitativeMeasurement) -> lGran == rGran
      (ATemporalAbsMeasurement, ATemporalAbsMeasurement)   -> lGran == rGran
      (ATemporalRelMeasurement, ATemporalRelMeasurement)   -> lGran == rGran
      _                                                    -> False

instance Show (MeasurementAndField In) where
  showsPrec d MeasurementAndField{mFieldType, mFilter} =
    showParen (d > app_prec) $
      showString "MeasurementAndField"
        <> showString "{ mFieldType=" <> showsPrec (app_prec + 1) mFieldType
        <> showString ", mFilter=" <> showsPrec (app_prec + 1) mFilter
        <> showString "}"
    where app_prec = 10

instance Show (MeasurementAndField Out) where
  showsPrec d MeasurementAndField{mFieldType, mFilter} =
    showParen (d > app_prec) $
      showString "MeasurementAndField"
        <> showString "{ mFieldType=" <> showsPrec (app_prec + 1) mFieldType
        <> showString ", mFilter=" <> showsPrec (app_prec + 1) mFilter
        <> showString "}"
    where app_prec = 10

instance Ord (MeasurementAndField In) where
  compare a b = compare (show a) (show b)

instance Ord (MeasurementAndField Out) where
  compare a b = compare (show a) (show b)

instance ToJSON (MeasurementAndField In) where
  toJSON MeasurementAndField{mFieldType, mFilter} =
    object [ "tag" .= mFieldType, "filter" .= mFilter]

instance FromJSON (MeasurementAndField In) where
  parseJSON = withObject "MeasurementAndField" $ \obj -> do
    tag <- obj .: "tag"
    case tag of
      SomeMeasurement ANominalMeasurement -> MeasurementAndField ANominalMeasurement <$> obj .: "filter"
      SomeMeasurement AnOrdinalMeasurement -> MeasurementAndField AnOrdinalMeasurement <$> obj .: "filter"
      SomeMeasurement AQuantitativeMeasurement -> MeasurementAndField AQuantitativeMeasurement <$> obj .: "filter"
      SomeMeasurement ATemporalAbsMeasurement -> MeasurementAndField ATemporalAbsMeasurement <$> obj .: "filter"
      SomeMeasurement ATemporalRelMeasurement -> MeasurementAndField ATemporalRelMeasurement <$> obj .: "filter"

instance ToJSON (MeasurementAndField Out) where
  toJSON MeasurementAndField{mFieldType, mFilter} =
    object [ "tag" .= mFieldType, "filter" .= mFilter]

instance FromJSON (MeasurementAndField Out) where
  parseJSON = withObject "MeasurementAndField" $ \obj -> do
    tag <- obj .: "tag"
    case tag of
      SomeMeasurement ANominalMeasurement -> MeasurementAndField ANominalMeasurement <$> obj .: "filter"
      SomeMeasurement AnOrdinalMeasurement -> MeasurementAndField AnOrdinalMeasurement <$> obj .: "filter"
      SomeMeasurement AQuantitativeMeasurement -> MeasurementAndField AQuantitativeMeasurement <$> obj .: "filter"
      SomeMeasurement ATemporalAbsMeasurement -> MeasurementAndField ATemporalAbsMeasurement <$> obj .: "filter"
      SomeMeasurement ATemporalRelMeasurement -> MeasurementAndField ATemporalRelMeasurement <$> obj .: "filter"

deriving  via (SerialiseViaJSON (MeasurementAndField In)) instance Serialise (MeasurementAndField In)
deriving  via (SerialiseViaJSON (MeasurementAndField Out)) instance Serialise (MeasurementAndField Out)

deriving  via (SerialiseViaJSON (FieldInMode In)) instance Serialise (FieldInMode In)
deriving  via (SerialiseViaJSON (FieldInMode Out)) instance Serialise (FieldInMode Out)

instance Show (FieldInMode In) where
  showsPrec d FieldInMode{sqlFieldName, fieldOptions, sortOrder, displayName} =
    showParen (d > app_prec) $
      showString "FieldInMode"
        <> showString "{ sqlFieldName=" <> showsPrec (app_prec + 1) sqlFieldName
        <> showString ", displayName=" <> showsPrec (app_prec + 1) displayName
        <> showString ", fieldOptions=" <> showsPrec (app_prec + 1) fieldOptions
        <> showString ", sortOrder=" <> showsPrec (app_prec + 1) sortOrder
        <> showString "}"
    where app_prec = 10

instance Relation (FieldInMode mode) where
  measurement = toMeasurement . fieldType . fieldOptions
  fieldName = Text.pack . getSqlFieldName . sqlFieldName
  fieldLabel k = maybe (fieldName k) id (displayName k)
  -- TODO: Implement @fieldLabel@ using display name. But we need to think about how the display
  --       name, which is part of the CLI configuration, can be made available to the
  --       part of the server that constructs the visualisation (where the labels are used)

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
