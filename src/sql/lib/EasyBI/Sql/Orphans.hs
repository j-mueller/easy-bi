{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# OPTIONS_GHC -Wno-orphans    #-}
module EasyBI.Sql.Orphans
  ( ReadShow (..)
  ) where

import Codec.Serialise               (Serialise (..))
import Data.Aeson                    (FromJSON (..), FromJSONKey, ToJSON (..),
                                      ToJSONKey)
import Language.SQL.SimpleSQL.Syntax (GroupingExpr, Name (..), QueryExpr,
                                      ScalarExpr, SetQuantifier, SortSpec,
                                      TableRef, TypeName)
import Text.Read                     (readMaybe)

deriving stock instance Ord Name

deriving via (ReadShow Name) instance Serialise Name

deriving via (ReadShow Name) instance ToJSON Name

deriving via (ReadShow Name) instance ToJSONKey Name

deriving via (ReadShow Name) instance FromJSON Name

deriving via (ReadShow Name) instance FromJSONKey Name

deriving via (ReadShow TypeName) instance Serialise TypeName

deriving via (ReadShow TypeName) instance ToJSON TypeName

deriving via (ReadShow TypeName) instance FromJSON TypeName

deriving via (ReadShow QueryExpr) instance Serialise QueryExpr

deriving via (ReadShow QueryExpr) instance ToJSON QueryExpr

deriving via (ReadShow QueryExpr) instance FromJSON QueryExpr

deriving via (ReadShow SetQuantifier) instance Serialise SetQuantifier

deriving via (ReadShow SetQuantifier) instance ToJSON SetQuantifier

deriving via (ReadShow SetQuantifier) instance FromJSON SetQuantifier

deriving via (ReadShow ScalarExpr) instance Serialise ScalarExpr

deriving via (ReadShow ScalarExpr) instance ToJSON ScalarExpr

deriving via (ReadShow ScalarExpr) instance FromJSON ScalarExpr

deriving via (ReadShow TableRef) instance Serialise TableRef

deriving via (ReadShow TableRef) instance ToJSON TableRef

deriving via (ReadShow TableRef) instance FromJSON TableRef

deriving via (ReadShow GroupingExpr) instance Serialise GroupingExpr

deriving via (ReadShow GroupingExpr) instance ToJSON GroupingExpr

deriving via (ReadShow GroupingExpr) instance FromJSON GroupingExpr

deriving via (ReadShow SortSpec) instance Serialise SortSpec

deriving via (ReadShow SortSpec) instance ToJSON SortSpec

deriving via (ReadShow SortSpec) instance FromJSON SortSpec

{-| Deriving serialise via read/show instance
-}
newtype ReadShow a = ReadShow{ unReadShow :: a }

instance (Read a, Show a) => Serialise (ReadShow a) where
  encode = encode . show . unReadShow
  decode = decode >>= fmap ReadShow . tryRead

instance (Show a) => ToJSON (ReadShow a) where
  toJSON (ReadShow a) = toJSON (show a)

instance (Read a) => FromJSON (ReadShow a) where
  parseJSON vl = parseJSON vl >>= fmap ReadShow . tryRead

tryRead :: (Read a, MonadFail m) => String -> m a
tryRead str = case readMaybe str of
    Nothing -> fail ("tryRead: 'read' failed for '" <> str <> "'")
    Just y  -> pure y

instance Show a => ToJSONKey (ReadShow a)

instance Read a => FromJSONKey (ReadShow a)
