{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Data source configuration
module EasyBI.Server.Config
  ( ConfigError (..)
  , DataSourceConfig (..)
  , DimensionGroupConfig (..)
  , FieldConfig (..)
  , cubesFromCatalog
  ) where

import Control.Lens                  qualified as L
import Data.Aeson                    (FromJSON, ToJSON)
import Data.Aeson                    qualified as JSON
import Data.Bifunctor                (Bifunctor (..))
import Data.Map                      (Map)
import Data.Map                      qualified as Map
import Data.Maybe                    (fromMaybe)
import Data.Text                     (Text)
import Data.Text                     qualified as Text
import EasyBI.Server.Cube            (Cube (..), CubeName (..),
                                      DimensionGroup (..))
import EasyBI.Server.Visualisation   (Dimension, Field (..), Measure (..),
                                      SortOrder (..), SqlFieldName (..),
                                      _dimensionDisplayName,
                                      _measureDisplayName, fields, sqlFieldName)
import EasyBI.Sql.Catalog            (Catalog (..), TypedQueryExpr (..))
import EasyBI.Sql.Effects.Types      (RowType (..), Tp (..), TyScheme (..))
import EasyBI.Util.JSON              (customJsonOptions)
import EasyBI.Util.NiceHash          (Plain, hPlain, withHash)
import GHC.Generics                  (Generic)
import Language.SQL.SimpleSQL.Syntax (Name (..))

data FieldConfig =
  FieldConfig
    { fcFieldName        :: Text
    , fcDisplayName      :: Maybe Text
    , fcDefaultSortOrder :: Maybe SortOrder
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON FieldConfig where
  toJSON = JSON.genericToJSON (customJsonOptions 2)
  toEncoding = JSON.genericToEncoding (customJsonOptions 2)

instance FromJSON FieldConfig where
  parseJSON = JSON.genericParseJSON (customJsonOptions 2)

data DimensionGroupConfig =
  DimensionGroupConfig
    { dgcName         :: Text
    , dgcDescription  :: Maybe Text
    , dgcPrimaryField :: FieldConfig
    , dgcOtherFields  :: [FieldConfig]
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON DimensionGroupConfig where
  toJSON = JSON.genericToJSON (customJsonOptions 3)
  toEncoding = JSON.genericToEncoding (customJsonOptions 3)

instance FromJSON DimensionGroupConfig where
  parseJSON = JSON.genericParseJSON (customJsonOptions 3)

data CubeConfig =
  CubeConfig
    { ccDimensions  :: [DimensionGroupConfig]
    , ccMeasures    :: [FieldConfig]
    , ccDisplayName :: Maybe String
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON CubeConfig where
  toJSON = JSON.genericToJSON (customJsonOptions 2)
  toEncoding = JSON.genericToEncoding (customJsonOptions 2)

instance FromJSON CubeConfig where
  parseJSON = JSON.genericParseJSON (customJsonOptions 2)

newtype DataSourceConfig =
  DataSourceConfig{ dscCubes :: Map CubeName CubeConfig}
    deriving stock (Eq, Show)
    deriving newtype (ToJSON, FromJSON)

data ConfigError =
  FieldNotFound Text
  -- ^ The field was not found in the data source
  | DataSourceNotFound CubeName
  -- ^ The data source with this name was not found
  | UnexpectedType Text
  -- ^ The view was found in the database, but it has an unexpected type
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Build a list of @DimensionGroup@s, throwing an error if the config specified a field
--   that does not exist in the input list.
configDimensionGroups :: Map SqlFieldName Field -> [DimensionGroupConfig] -> Either ConfigError [DimensionGroup]
configDimensionGroups fieldsByName dscDimensionGroups = do
  let dimensionByName :: FieldConfig -> Either ConfigError Dimension
      dimensionByName FieldConfig{fcFieldName, fcDisplayName, fcDefaultSortOrder} = case Map.lookup (SqlFieldName $ Text.unpack fcFieldName) fieldsByName of
        Nothing              -> Left (FieldNotFound fcFieldName)
        Just (ADimension d)  -> Right $ L.set _dimensionDisplayName fcDisplayName d
        Just AMeasure{}      -> Left (UnexpectedType fcFieldName)
      groupFromConfig DimensionGroupConfig{dgcName, dgcDescription, dgcPrimaryField, dgcOtherFields} =
        DimensionGroup dgcName dgcDescription
          <$> dimensionByName dgcPrimaryField
          <*> traverse dimensionByName dgcOtherFields
  traverse groupFromConfig dscDimensionGroups

configMeasure :: Map SqlFieldName Field -> [FieldConfig] -> Either ConfigError [Measure]
configMeasure fieldsByName fieldConfigs =
  let dimensionByName :: FieldConfig -> Either ConfigError Measure
      dimensionByName FieldConfig{fcFieldName, fcDisplayName, fcDefaultSortOrder} = case Map.lookup (SqlFieldName $ Text.unpack fcFieldName) fieldsByName of
        Nothing           -> Left (FieldNotFound fcFieldName)
        Just ADimension{} -> Left (UnexpectedType fcFieldName)
        Just (AMeasure m) -> Right $ L.set _measureDisplayName fcDisplayName m
  in traverse dimensionByName fieldConfigs

-- | Build a list of cubes from a database catalog and a data source config.
cubesFromCatalog :: Catalog -> DataSourceConfig -> Either ConfigError [Cube Plain]
cubesFromCatalog Catalog{_views} DataSourceConfig{dscCubes} = do
  let availableViews = Map.fromList $ fmap (first (CubeName . Text.unpack . mkTitle)) $ Map.toList _views
      mkTitle [] = "<no title>"
      mkTitle xs =
        let get (Name _ n) = Text.pack n
        in Text.intercalate "." (get <$> xs)
      mkCube :: CubeName -> CubeConfig -> Either ConfigError (Cube Plain)
      mkCube cubeName CubeConfig{ccDimensions, ccDisplayName, ccMeasures} = do
        let CubeName viewName = cubeName
        typedQueryExpr <- maybe (Left $ DataSourceNotFound cubeName) Right (Map.lookup cubeName availableViews)
        fields_ <- case teType typedQueryExpr of { TyScheme _ (TpRow (RowType _ mp)) -> Right (fields mp); x -> Left (UnexpectedType $ Text.pack $ show x)}
        let fieldsByName = Map.fromList $ fmap (\fld -> (sqlFieldName fld, fld)) fields_
        Cube
          <$> pure (hPlain typedQueryExpr)
          <*> pure cubeName
          <*> pure (fromMaybe viewName ccDisplayName)
          <*> fmap (fmap withHash) (configDimensionGroups fieldsByName ccDimensions)
          <*> fmap (fmap withHash) (configMeasure fieldsByName ccMeasures)

  traverse (uncurry mkCube) (Map.toList dscCubes)
