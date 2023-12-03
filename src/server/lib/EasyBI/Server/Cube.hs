{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module EasyBI.Server.Cube
  ( Cube (..)
  , CubeName (..)
  , DimensionGroup (..)
  , fields
  , hashCube
  , queryExpr
  , singletonDimensionGroup
  ) where

import Codec.Serialise             (Serialise (..))
import Data.Aeson                  (FromJSON (..), FromJSONKey, ToJSON (..),
                                    ToJSONKey)
import Data.Aeson                  qualified as JSON
import Data.Map                    (Map)
import Data.Map                    qualified as Map
import Data.Text                   (Text)
import Data.Text                   qualified as Text
import EasyBI.Server.Visualisation (Dimension (..), Field (..), Measure,
                                    SqlFieldName (..), Visualisation,
                                    sqlFieldName)
import EasyBI.Sql.Catalog          (TypedQueryExpr)
import EasyBI.Util.JSON            (customJsonOptions)
import EasyBI.Util.NiceHash        (HasNiceHash (..), Hashable (..), Hashed,
                                    NiceHash, NiceHashable (..), Plain,
                                    WithHash, hHash)
import GHC.Generics                (Generic)

newtype CubeName = CubeName String
  deriving stock (Eq, Ord, Show)
  deriving newtype (ToJSON, ToJSONKey, FromJSON, FromJSONKey, Serialise)

{-| A cube is a big SELECT statement with all dimensions
and aggregations
-}
data Cube h =
  Cube
    { cQuery       :: Hashable TypedQueryExpr h
    , cName        :: CubeName
    , cDisplayName :: String
    , cDimensions  :: [WithHash DimensionGroup]
    , cMeasures    :: [WithHash Measure]
    } deriving stock Generic

instance HasNiceHash (Visualisation (NiceHash (Cube Hashed))) where
  type Name (Visualisation (NiceHash (Cube Hashed))) = "vis"

instance ToJSON (Cube Plain) where
  toJSON = JSON.genericToJSON (customJsonOptions 1)
  toEncoding = JSON.genericToEncoding (customJsonOptions 1)

instance FromJSON (Cube Plain) where
  parseJSON = JSON.genericParseJSON (customJsonOptions 1)

instance ToJSON (Cube Hashed) where
  toJSON = JSON.genericToJSON (customJsonOptions 1)
  toEncoding = JSON.genericToEncoding (customJsonOptions 1)

instance FromJSON (Cube Hashed) where
  parseJSON = JSON.genericParseJSON (customJsonOptions 1)

deriving instance Serialise (Cube Hashed)
deriving instance Serialise (Cube Plain)

hashCube :: Cube h -> Cube Hashed
hashCube cube = cube{cQuery = hHash (cQuery cube)}

queryExpr :: Cube Plain -> TypedQueryExpr
queryExpr Cube{cQuery} = let HPlain a = cQuery in a

instance HasNiceHash (Cube Hashed) where
  type Name (Cube Hashed) = "cube"

data DimensionGroup =
  DimensionGroup
    { dgName             :: Text
    , dgDescription      :: Maybe Text
    , dgPrimaryDimension :: Dimension
    , dgOtherDimensions  :: [Dimension]
    } deriving stock (Eq, Generic)
      deriving anyclass (Serialise)
      deriving HasNiceHash via (NiceHashable "dimension_group" DimensionGroup)

instance ToJSON DimensionGroup where
  toJSON = JSON.genericToJSON (customJsonOptions 2)
  toEncoding = JSON.genericToEncoding (customJsonOptions 2)

instance FromJSON DimensionGroup where
  parseJSON = JSON.genericParseJSON (customJsonOptions 2)

-- | A field group with a single field
singletonDimensionGroup :: Dimension -> DimensionGroup
singletonDimensionGroup f =
  DimensionGroup
    (Text.pack $ getSqlFieldName $ dimensionSqlFieldName f)
    Nothing
    f
    []

-- | The fields that have been defined for the cube
fields :: Cube h -> Map SqlFieldName Field
fields Cube{cDimensions, cMeasures} =
  let withKey f@Dimension{dimensionSqlFieldName} = (dimensionSqlFieldName, ADimension f)
      mkMap DimensionGroup{dgPrimaryDimension, dgOtherDimensions} =
        Map.fromList $ fmap withKey (dgPrimaryDimension : dgOtherDimensions)
      mkField f = (sqlFieldName f, f)
  in Map.unions $ (Map.fromList $ fmap (mkField . AMeasure . snd) cMeasures) : (mkMap . snd <$> cDimensions)
