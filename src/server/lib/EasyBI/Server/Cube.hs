{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module EasyBI.Server.Cube
  ( Cube (..)
  , hashCube
  , queryExpr
  ) where

import Codec.Serialise      (Serialise (..))
import Data.Aeson           (FromJSON (..), ToJSON (..))
import EasyBI.Sql.Catalog   (TypedQueryExpr)
import EasyBI.Util.NiceHash (HasNiceHash (..), Hashable (..), Hashed, Plain,
                             hHash)
import GHC.Generics         (Generic)

{-| A cube is a big SELECT statement with all dimensions
and aggregations
-}
data Cube h =
  Cube
    { cQuery :: Hashable TypedQueryExpr h
    , cTitle :: String
    } deriving stock Generic

deriving instance FromJSON (Cube Hashed)
deriving instance FromJSON (Cube Plain)

deriving instance ToJSON (Cube Hashed)
deriving instance ToJSON (Cube Plain)

deriving instance Serialise (Cube Hashed)
deriving instance Serialise (Cube Plain)

hashCube :: Cube h -> Cube Hashed
hashCube cube = cube{cQuery = hHash (cQuery cube)}

queryExpr :: Cube Plain -> TypedQueryExpr
queryExpr Cube{cQuery} = let HPlain a = cQuery in a

instance HasNiceHash (Cube Hashed) where
  type Name (Cube Hashed) = "cube"
