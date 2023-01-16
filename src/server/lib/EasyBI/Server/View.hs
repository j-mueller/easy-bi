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
module EasyBI.Server.View
  ( View (..)
  , hashView
  , queryExpr
  ) where

import Codec.Serialise      (Serialise (..))
import Data.Aeson           (FromJSON (..), ToJSON (..))
import EasyBI.Sql.Catalog   (TypedQueryExpr)
import EasyBI.Util.NiceHash (HasNiceHash (..), Hashable (..), Hashed, Plain,
                             hHash)
import GHC.Generics         (Generic)

{-| A view is a single chart that is displayed to the user. It contains
a SQL query, a hole specification and a visualisation.
-}
data View h =
  View
    { vQuery :: Hashable TypedQueryExpr h
    , vTitle :: String
    } deriving stock Generic

deriving instance FromJSON (View Hashed)
deriving instance FromJSON (View Plain)

deriving instance ToJSON (View Hashed)
deriving instance ToJSON (View Plain)

deriving instance Serialise (View Hashed)
deriving instance Serialise (View Plain)

hashView :: View h -> View Hashed
hashView view = view{vQuery = hHash (vQuery view)}

queryExpr :: View Plain -> TypedQueryExpr
queryExpr View{vQuery} = let HPlain a = vQuery in a

instance HasNiceHash (View Hashed) where
  type Name (View Hashed) = "view"
