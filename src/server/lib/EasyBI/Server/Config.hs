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
  , FieldConfig (..)
  , FieldGroupConfig (..)
  , configFieldGroups
  , cubesFromCatalog
  ) where

import Control.Lens                  qualified as L
import Data.Aeson                    (FromJSON, ToJSON)
import Data.Aeson                    qualified as JSON
import Data.Bifunctor                (Bifunctor (..))
import Data.Map                      (Map)
import Data.Map                      qualified as Map
import Data.Text                     (Text)
import Data.Text                     qualified as Text
import EasyBI.Server.Cube            (Cube (..), FieldGroup (..))
import EasyBI.Server.Visualisation   (FieldInMode (..), InOut (..),
                                      SortOrder (..), SqlFieldName (..),
                                      _displayName, _sortOrder, fields)
import EasyBI.Sql.Catalog            (Catalog (..), TypedQueryExpr (..))
import EasyBI.Sql.Effects.Types      (RowType (..), Tp (..), TyScheme (..))
import EasyBI.Util.JSON              (customJsonOptions)
import EasyBI.Util.NiceHash          (Plain, hPlain)
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

data FieldGroupConfig =
  FieldGroupConfig
    { fgcName         :: Text
    , fgcDescription  :: Maybe Text
    , fgcPrimaryField :: FieldConfig
    , fgcOtherFields  :: [FieldConfig]
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON FieldGroupConfig where
  toJSON = JSON.genericToJSON (customJsonOptions 3)
  toEncoding = JSON.genericToEncoding (customJsonOptions 3)

instance FromJSON FieldGroupConfig where
  parseJSON = JSON.genericParseJSON (customJsonOptions 3)

newtype DataSourceConfig =
  DataSourceConfig
    { dscFieldGroups :: Map Text [FieldGroupConfig]
    }
    deriving stock (Eq, Show)
    deriving newtype (ToJSON, FromJSON)

data ConfigError =
  FieldNotFound Text
  -- ^ The field was not found in the data source
  | DataSourceNotFound Text
  -- ^ The data source with this name was not found
  | UnexpectedType Text
  -- ^ The view was found in the database, but it has an unexpected type
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Build a list of @FieldGroup@s, throwing an error if the config specified a field
--   that does not exist in the input list.
configFieldGroups :: [FieldInMode Out] -> [FieldGroupConfig] -> Either ConfigError [FieldGroup]
configFieldGroups allFields dscFieldGroups = do
  let fieldsByName = Map.fromList $ fmap (\fld -> (sqlFieldName fld, fld)) allFields
      fieldByName FieldConfig{fcFieldName, fcDisplayName, fcDefaultSortOrder} = case Map.lookup (SqlFieldName $ Text.unpack fcFieldName) fieldsByName of
        Nothing              -> Left (FieldNotFound fcFieldName)
        Just x@FieldInMode{} -> Right $
          L.set _displayName fcDisplayName
          . maybe id (\o -> L.set _sortOrder o) fcDefaultSortOrder
          $ x
      groupFromConfig FieldGroupConfig{fgcName, fgcDescription, fgcPrimaryField, fgcOtherFields} =
        FieldGroup fgcName fgcDescription
          <$> fieldByName fgcPrimaryField
          <*> traverse fieldByName fgcOtherFields
  traverse groupFromConfig dscFieldGroups

-- | Build a list of cubes from a database catalog and a data source config.
cubesFromCatalog :: Catalog -> DataSourceConfig -> Either ConfigError [Cube Plain]
cubesFromCatalog Catalog{_views} DataSourceConfig{dscFieldGroups} = do
  let availableViews = Map.fromList $ fmap (first mkTitle) $ Map.toList _views
      mkTitle [] = "<no title>"
      mkTitle xs =
        let get (Name _ n) = Text.pack n
        in Text.intercalate "." (get <$> xs)
      mkCube viewName fieldGroups = do
        typedQueryExpr <- maybe (Left $ DataSourceNotFound viewName) Right (Map.lookup viewName availableViews)
        fields_ <- case teType typedQueryExpr of { TyScheme _ (TpRow (RowType _ mp)) -> Right (fields mp); x -> Left (UnexpectedType $ Text.pack $ show x)}
        Cube
          <$> pure (hPlain typedQueryExpr)
          <*> pure (Text.unpack viewName)
          <*> pure (Text.unpack viewName) -- FIXME:_ Display name in config
          <*> configFieldGroups fields_ fieldGroups

  traverse (uncurry mkCube) (Map.toList dscFieldGroups)
