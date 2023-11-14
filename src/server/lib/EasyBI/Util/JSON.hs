{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE ViewPatterns       #-}
module EasyBI.Util.JSON
  ( SerialiseViaJSON (..)
  , WrappedObject (..)
  , _SerialiseViaJSON
  , _WrappedObject
  , customJsonOptions
  , fromValue
  ) where

import Codec.Serialise                  (Serialise (..))
import Control.Lens                     (Iso', iso)
import Data.Aeson                       (FromJSON (..), Object, ToJSON (..))
import Data.Aeson                       qualified as Aeson
import Data.Aeson.Encode.Pretty         qualified as Pretty
import Data.ByteString.Lazy             qualified as BSL
import Data.Text                        qualified as Text
import Data.Text.Encoding               qualified as Text
import Database.SQLite.Simple           (SQLData (..))
import Database.SQLite.Simple.FromField (FromField (..), fieldData)

newtype WrappedObject = WrappedObject Object
  deriving newtype (ToJSON, FromJSON)

instance Serialise WrappedObject where
  encode (WrappedObject obj) = encode (Aeson.encode $ Aeson.Object obj)
  decode = fmap Aeson.eitherDecode decode >>= \case
    Left err -> fail (show err)
    Right (Aeson.Object obj) -> pure (WrappedObject obj)
    Right _vl -> fail ("WrappedObject.decode: unexpected JSON value")

instance Show WrappedObject where
  show obj =
    Text.unpack $ Text.decodeUtf8 $ BSL.toStrict $ Pretty.encodePretty obj

-- | Deriving a @Serialise@ instance from the @ToJSON@ & @FromJSON@ instances
newtype SerialiseViaJSON a = SerialiseViaJSON{ unSerialiseViaJSON :: a}

instance (ToJSON a, FromJSON a) => Serialise (SerialiseViaJSON a) where
  encode = encode . Aeson.encode . unSerialiseViaJSON
  decode = decode >>= either fail (pure . SerialiseViaJSON) . Aeson.eitherDecode

_SerialiseViaJSON :: Iso' (SerialiseViaJSON a) a
_SerialiseViaJSON = iso unSerialiseViaJSON SerialiseViaJSON

fromValue :: Aeson.Value -> Maybe WrappedObject
fromValue (Aeson.Object obj) = Just (WrappedObject obj)
fromValue _                  = Nothing

instance FromField WrappedObject where
  fromField (fieldData -> SQLText t) = case Aeson.eitherDecode (BSL.fromStrict $ Text.encodeUtf8 t) of
    Left err -> fail (show err)
    Right k  -> pure k
  fromField f                        =
    fail ("FromField WrappedObject: fromField: expected SQLText but got: " <> show (fieldData f))

_WrappedObject :: Iso' WrappedObject Object
_WrappedObject = iso (\(WrappedObject o) -> o) WrappedObject

-- | JSON options that drop @n@ characters and then apply @Aeson.camel2@ to the rest
customJsonOptions :: Int -> Aeson.Options
customJsonOptions i = Aeson.defaultOptions{Aeson.fieldLabelModifier= Aeson.camelTo2 '_' . drop i }
