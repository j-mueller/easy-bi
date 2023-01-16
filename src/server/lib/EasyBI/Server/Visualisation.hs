{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-| Visualisations
-}
module EasyBI.Server.Visualisation
  ( Visualisation (..)
  , visualisations
  ) where

import Data.Aeson               (FromJSON (..), ToJSON (..))
import EasyBI.Sql.Effects.Types (Tp, TyVar)
import EasyBI.Util.JSON         (WrappedObject)
import GHC.Generics             (Generic)

data Visualisation =
  Visualisation
    { visDefinition  :: WrappedObject
    , visDescription :: String
    }
    deriving stock Generic
    deriving anyclass (ToJSON, FromJSON)

visualisations :: Tp TyVar -> [Visualisation]
visualisations _ = []
