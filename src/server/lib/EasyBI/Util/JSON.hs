{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
module EasyBI.Util.JSON
  ( WrappedObject (..)
  , fromValue
  ) where

import Codec.Serialise          (Serialise (..))
import Data.Aeson               (FromJSON (..), Object, ToJSON (..))
import Data.Aeson               qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Pretty
import Data.ByteString.Lazy     qualified as BSL
import Data.Text                qualified as Text
import Data.Text.Encoding       qualified as Text

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

fromValue :: Aeson.Value -> Maybe WrappedObject
fromValue (Aeson.Object obj) = Just (WrappedObject obj)
fromValue _                  = Nothing
