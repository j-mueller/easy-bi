{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-| Visualisations
-}
module EasyBI.Server.Visualisation
  ( Field (..)
  , Visualisation (..)
  , visualisations
  ) where

import Control.Lens                  ((&), (.~))
import Data.Aeson                    (FromJSON (..), ToJSON (..))
import Data.Bifunctor                (Bifunctor (..))
import Data.Map                      (Map)
import Data.Map                      qualified as Map
import Data.Maybe                    (mapMaybe)
import Data.Text                     qualified as Text
import EasyBI.Sql.Effects.Types      (RowType (..), SqlType (..), Tp (..),
                                      TyScheme (..), TyVar)
import EasyBI.Util.JSON              (WrappedObject (..), fromValue)
import EasyBI.Vis.HVega              qualified as HVega
import EasyBI.Vis.Rules              (makeChart)
import EasyBI.Vis.Types              (Encoding, Measurement (..), Relation (..),
                                      Selections, emptySelections, runRule,
                                      wildCards)
import GHC.Generics                  (Generic)
import Language.SQL.SimpleSQL.Syntax (Name (..))


data Visualisation =
  Visualisation
    { visDefinition  :: WrappedObject
    , visDescription :: String
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

visualisations :: TyScheme TyVar (Tp TyVar) -> [Visualisation]
visualisations = maybe [] (mapMaybe enc . runRule makeChart) . selections where

selections :: TyScheme TyVar (Tp TyVar) -> Maybe (Selections Field)
selections (TyScheme _ (TpRow (RowType _ mp))) = Just (fields mp)
selections _                                   = Nothing

fields :: Map Name (Tp TyVar) -> Selections Field
fields mp = emptySelections & wildCards .~ wcs where
  getName (Name _ n) = n
  getMeasure (TpSql t) = case t of
    STNumber   -> Just Quantitative
    STInt      -> Just Ordinal -- TODO: Could be quant. sometimes?
    STText     -> Just Nominal
    STBool     -> Just Nominal
    STDateTime -> Just TemporalAbs
    _          -> Nothing
  getMeasure _ = Nothing
  wcs = mapMaybe (fmap (uncurry Field . first getName) . traverse getMeasure) (Map.toList mp)

enc :: Encoding Field -> Maybe Visualisation
enc e =
  Visualisation <$> fromValue (HVega.toJSON e) <*> pure "FIXME: enc.visDescription"

{-| A field with a measurement
-}
data Field = Field{ name :: String, fieldType :: Measurement }
  deriving (Eq)

instance Relation Field where
  measurement = fieldType
  fieldName = Text.pack . name
