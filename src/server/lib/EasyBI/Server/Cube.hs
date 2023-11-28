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
  , FieldGroup (..)
  , hashCube
  , queryExpr
  , singletonFieldGroup
  ) where

import Codec.Serialise             (Serialise (..))
import Data.Aeson                  (FromJSON (..), ToJSON (..))
import Data.Aeson                  qualified as JSON
import Data.Text                   (Text)
import Data.Text                   qualified as Text
import EasyBI.Server.Visualisation (FieldInMode (..), InOut (Out),
                                    SqlFieldName (..))
import EasyBI.Sql.Catalog          (TypedQueryExpr)
import EasyBI.Util.JSON            (customJsonOptions)
import EasyBI.Util.NiceHash        (HasNiceHash (..), Hashable (..), Hashed,
                                    NiceHashable (..), Plain, WithHash, hHash)
import GHC.Generics                (Generic)

{-| A cube is a big SELECT statement with all dimensions
and aggregations
-}
data Cube h =
  Cube
    { cQuery       :: Hashable TypedQueryExpr h
    , cName        :: String
    , cDisplayName :: String
    , cFields      :: [WithHash FieldGroup]
    } deriving stock Generic

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

data FieldGroup =
  FieldGroup
    { fgName         :: Text
    , fgDescription  :: Maybe Text
    , fgPrimaryField :: FieldInMode Out
    , fgOtherFields  :: [FieldInMode Out]
    } deriving stock (Eq, Generic)
      deriving anyclass (Serialise)
      deriving HasNiceHash via (NiceHashable "field_group" FieldGroup)

instance ToJSON FieldGroup where
  toJSON = JSON.genericToJSON (customJsonOptions 2)
  toEncoding = JSON.genericToEncoding (customJsonOptions 2)

instance FromJSON FieldGroup where
  parseJSON = JSON.genericParseJSON (customJsonOptions 2)

-- | A field group with a single field
singletonFieldGroup :: FieldInMode Out -> FieldGroup
singletonFieldGroup f = FieldGroup (Text.pack $ getSqlFieldName $ sqlFieldName f) Nothing f []
