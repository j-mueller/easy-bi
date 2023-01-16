{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DerivingStrategies      #-}
{-# LANGUAGE DerivingVia             #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
module EasyBI.Util.NiceHash
  ( HasNiceHash (..)
  , NiceHash (..)
  , NiceHashable (..)
  , TestData (..)
  , mkNiceHash
    -- * Hashable
  , Hashable (..)
  , Hashed
  , Plain
  , WithHash
  , hHash
  , hNiceHash
  , hPlain
  , withHash
  ) where

import Codec.Serialise          (Serialise (..), serialise)
import Control.DeepSeq          (NFData)
import Crypto.Hash.SHA256       qualified as Hash
import Data.Aeson               (FromJSON (..), ToJSON (..))
import Data.Binary              (Binary (..))
import Data.ByteString.Lazy     qualified as BSL
import Data.Proxy               (Proxy (..))
import Data.Text                (Text)
import Data.Text                qualified as Text
import EasyBI.Sql.Catalog       (TypedQueryExpr (..))
import GHC.Generics             (Generic)
import GHC.TypeLits             (KnownSymbol, Symbol, symbolVal)
import Prettyprinter            (Pretty (..))
import Text.Hex qualified
import Web.Internal.HttpApiData (FromHttpApiData)

newtype NiceHash a = NiceHash { unNiceHash :: Text }
  deriving stock (Eq, Ord)
  deriving newtype
    ( Pretty,
      Serialise,
      ToJSON,
      FromJSON,
      FromHttpApiData,
      Binary,
      NFData
    )

instance Show (NiceHash a) where
  show = Text.unpack . unNiceHash

mkNiceHash :: forall (s :: Symbol) a. KnownSymbol s => Serialise a => a -> NiceHash a
mkNiceHash =
  let prefix = Text.pack (symbolVal $ Proxy @s) <> "_" in
  NiceHash . (prefix <>) . Text.Hex.encodeHex . Hash.hash . BSL.toStrict . serialise

newtype NiceHashable (name :: Symbol) a = NiceHashable{ unNiceHashable :: a }
  deriving newtype Serialise

instance (KnownSymbol name, Serialise a) => HasNiceHash (NiceHashable name a) where
  type Name (NiceHashable name a) = name

class (Serialise a, KnownSymbol (Name a)) => HasNiceHash a where
    type Name a :: Symbol

    niceHash :: a -> NiceHash a
    niceHash = mkNiceHash @(Name a)

data Hashed
data Plain

{-| Values that can be hashed
-}
data Hashable a h where
  HHash  :: NiceHash a -> Hashable a Hashed
  HPlain :: a          -> Hashable a Plain

instance Serialise a => Serialise (Hashable a Plain) where
  encode (HPlain x) = encode x
  decode = HPlain <$> decode

instance Serialise (Hashable a Hashed) where
  encode (HHash x) = encode x
  decode = HHash <$> decode

instance ToJSON a => ToJSON (Hashable a Plain) where
  toJSON (HPlain x) = toJSON x

instance FromJSON a => FromJSON (Hashable a Plain) where
  parseJSON x = HPlain <$> parseJSON x

instance ToJSON (Hashable a Hashed) where
  toJSON (HHash x) = toJSON x

instance FromJSON (Hashable a Hashed) where
  parseJSON x = HHash <$> parseJSON x

deriving stock instance Show a => Show (Hashable a h)

hPlain :: a -> Hashable a Plain
hPlain = HPlain

hHash :: HasNiceHash a => Hashable a h -> Hashable a Hashed
hHash = \case
  HHash h  -> HHash h
  HPlain x -> HHash (niceHash x)

hNiceHash :: HasNiceHash a => Hashable a h -> NiceHash a
hNiceHash k =
  let HHash r = hHash k
  in r

data TestData = TestData{ a :: String, b :: Int}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Serialise)
  deriving HasNiceHash via (NiceHashable "test" TestData)

deriving via (NiceHashable "text" Text) instance HasNiceHash Text

deriving via (NiceHashable "typed_query" TypedQueryExpr) instance HasNiceHash TypedQueryExpr

type WithHash a = (NiceHash a, a)

withHash :: HasNiceHash a => a -> WithHash a
withHash a' = (niceHash a', a')
