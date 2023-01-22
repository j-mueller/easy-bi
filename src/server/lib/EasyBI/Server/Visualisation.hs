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
import Data.List                     (sortOn)
import Data.Map                      (Map)
import Data.Map                      qualified as Map
import Data.Maybe                    (mapMaybe)
import Data.Ord                      (Down (..))
import Data.Text                     qualified as Text
import EasyBI.Sql.Effects.Types      (RowType (..), SqlType (..), Tp (..),
                                      TyScheme (..), TyVar)
import EasyBI.Util.JSON              (WrappedObject (..), fromValue)
import EasyBI.Vis.HVega              qualified as HVega
import EasyBI.Vis.Rules              (makeChart)
import EasyBI.Vis.Types              (Encoding, Measurement (..), Relation (..),
                                      Score (..), Selections, emptySelections,
                                      runRule, score, wildCards)
import GHC.Generics                  (Generic)
import Language.SQL.SimpleSQL.Syntax (Name (..))

{-| Visualisation to be shown on the client
-}
data Visualisation =
  Visualisation
    { visDefinition  :: WrappedObject
    -- ^ Specification of the graph in HVega
    , visDescription :: String
    -- ^ Description
    , visScore       :: Score
    -- ^ Score
    }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON, FromJSON)

visualisations :: TyScheme TyVar (Tp TyVar) -> [Visualisation]
visualisations =
  let addScore x = traverse score (x, x) in
  maybe []
    (mapMaybe (uncurry enc)
      . sortOn (Down . snd)
      . mapMaybe addScore
      . runRule makeChart)
    . selections

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

enc :: Encoding Field -> Score -> Maybe Visualisation
enc e s =
  Visualisation <$> fromValue (HVega.toJSON e) <*> pure "FIXME: enc.visDescription" <*> pure s

{-| A field with a measurement
-}
data Field = Field{ name :: String, fieldType :: Measurement }
  deriving (Eq)

instance Relation Field where
  measurement = fieldType
  fieldName = Text.pack . name
