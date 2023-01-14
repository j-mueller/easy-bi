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
  , WrappedObject (..)
  , WrappedQueryExpr (..)
  , hashView
  , queryExpr
  ) where

import Codec.Serialise                (Serialise (..))
import Data.Aeson                     (FromJSON (..), Object, ToJSON (..))
import Data.Aeson                     qualified as Aeson
import Data.Text                      qualified as Text
import EasyBI.Util.NiceHash           (HasNiceHash (..), Hashable (..), Hashed,
                                       Plain, hHash)
import GHC.Generics                   (Generic)
import Language.SQL.SimpleSQL.Dialect qualified as Dialect
import Language.SQL.SimpleSQL.Parse   qualified as SQL
import Language.SQL.SimpleSQL.Pretty  qualified as Pretty
import Language.SQL.SimpleSQL.Syntax  (QueryExpr)

{-| A view is a single chart that is displayed to the user. It contains
a SQL query, a hole specification and a visualisation.
-}
data View h =
  View
    { vQuery         :: Hashable WrappedQueryExpr h
    , vVisualisation :: WrappedObject
    , vTitle         :: String
    } deriving stock Generic

deriving instance FromJSON (View Hashed)
deriving instance FromJSON (View Plain)

deriving instance ToJSON (View Hashed)
deriving instance ToJSON (View Plain)

deriving instance Serialise (View Hashed)
deriving instance Serialise (View Plain)

hashView :: View h -> View Hashed
hashView view = view{vQuery = hHash (vQuery view)}

queryExpr :: View Plain -> WrappedQueryExpr
queryExpr View{vQuery} = let HPlain a = vQuery in a

instance Serialise WrappedQueryExpr where
  encode (WrappedQueryExpr e) = encode (Pretty.prettyQueryExpr Dialect.postgres e)
  decode = fmap (SQL.parseQueryExpr Dialect.postgres "" Nothing) decode >>= \case
    Left err  -> fail (show err)
    Right qry -> pure (WrappedQueryExpr qry)

newtype WrappedQueryExpr = WrappedQueryExpr{ unWrappedQuery :: QueryExpr }

newtype WrappedObject = WrappedObject Object
  deriving newtype (ToJSON, FromJSON)

instance Serialise WrappedObject where
  encode (WrappedObject obj) = encode (Aeson.encode $ Aeson.Object obj)
  decode = fmap Aeson.eitherDecode decode >>= \case
    Left err -> fail (show err)
    Right (Aeson.Object obj) -> pure (WrappedObject obj)
    Right _vl -> fail ("WrappedObject.decode: unexpected JSON value")

instance ToJSON WrappedQueryExpr where
  toJSON (WrappedQueryExpr e) = toJSON (Pretty.prettyQueryExpr Dialect.postgres e)

instance FromJSON WrappedQueryExpr where
  parseJSON (Aeson.String text) =
    case SQL.parseQueryExpr Dialect.postgres "" Nothing (Text.unpack text) of
      Left err -> fail (show err)
      Right x  -> pure (WrappedQueryExpr x)
  parseJSON _ = fail "WrappedQueryExpr: Expected string"

instance HasNiceHash WrappedQueryExpr where
  type Name WrappedQueryExpr = "query"

instance HasNiceHash (View Hashed) where
  type Name (View Hashed) = "view"
