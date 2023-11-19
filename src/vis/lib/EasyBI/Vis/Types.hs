{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE UndecidableInstances   #-}

module EasyBI.Vis.Types
  ( Mark (..)
  , Measurement (..)
    -- * Relations
  , Relation (..)
    -- * Archetypes
  , Archetype (..)
  , archetype
    -- * User selections
  , Selections (..)
  , color
  , emptySelections
  , initialSelections
  , selectedArchetype
  , selectedMark
  , wildCards
  , xAxis
  , yAxis
  , yAxis2
    -- * Rules
  , Rule
  , runRule
  ) where

import Codec.Serialise           (Serialise)
import Control.Lens              (makeLenses)
import Control.Monad.Logic       qualified as LogicT
import Control.Monad.Logic.Class (MonadLogic)
import Control.Monad.Reader      (MonadReader, runReaderT)
import Data.Aeson                (FromJSON, ToJSON)
import Data.Text                 (Text)
import EasyBI.Vis.Charts         (Chart)
import EasyBI.Vis.Charts         qualified as Charts
import GHC.Generics              (Generic)

{-| Class of relations between fields (from the Mackinlay paper)
-}
class Relation a where
  measurement :: a -> Measurement
  fieldName :: a -> Text
  fieldLabel :: a -> Text
  fieldLabel = fieldName

{-| Things that can be measured
-}
data Measurement
  = Nominal -- ^ Names (string)
  | Ordinal -- ^ Things with an order (integer)
  | Quantitative -- ^ Quantities (double)
  | TemporalAbs -- ^ Absolute temporal values (UTCTime)
  | TemporalRel -- ^ Relative temporal values (day-of-week, week-of-month, quarter-of-year, etc.)
  -- | Geofeature
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

data Mark = Bar | Point | Line | Rect
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON, Serialise)

{-| Visualisation archetype. This is only used for UX purposes
(showing a symbol to the user)
-}
data Archetype =
  HorizontalBarChart
  | VerticalBarChart
  | Linechart
  | Scatterplot
  | Heatmap
  | Misc
  deriving stock (Eq, Ord, Show, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON, Serialise)

-- | Data submitted by the user
data Selections k f =
  Selections
    { _WildCards         :: [f]
    , _XAxis             :: k f
    , _YAxis             :: k f
    , _YAxis2            :: k f
    , _Color             :: k f
    , _selectedMark      :: k Mark
    , _selectedArchetype :: k Archetype
    } deriving stock Generic

deriving instance Eq f => Eq (Selections [] f)
deriving instance Eq f => Eq (Selections Maybe f)

deriving instance (ToJSON f, ToJSON (k f), ToJSON (k Mark), ToJSON (k Archetype)) => ToJSON (Selections k f)
deriving instance (FromJSON f, FromJSON (k f), FromJSON (k Mark), FromJSON (k Archetype)) => FromJSON (Selections k f)

deriving instance Show f => Show (Selections [] f)
deriving instance Show f => Show (Selections Maybe f)

instance Foldable k => Foldable (Selections k) where
  foldMap f selections =
    foldMap f (_WildCards selections)
    <> foldMap f (_XAxis selections)
    <> foldMap f (_YAxis selections)
    <> foldMap f (_YAxis2 selections)
    <> foldMap f (_Color selections)

{-| Turn a 'Selections []' object, with possible choices for
specific channels, into a 'Selections Maybe' object, in which
each channel is either set to a specific value or not set at
all
-}
initialSelections :: Selections [] f -> [Selections Maybe f]
initialSelections sel = do
  let maybeList :: [a] -> [Maybe a]
      maybeList [] = [Nothing]
      maybeList xs = Just <$> xs
  xAxis <- maybeList (_XAxis sel)
  yAxis <- maybeList (_YAxis sel)
  yAxis2 <- maybeList (_YAxis2 sel)
  color <- maybeList (_Color sel)
  mk <- maybeList (_selectedMark sel)
  archetype <- maybeList (_selectedArchetype sel)
  pure
    Selections
      { _WildCards = _WildCards sel
      , _XAxis = xAxis
      , _YAxis = yAxis
      , _YAxis2 = yAxis2
      , _Color = color
      , _selectedMark = mk
      , _selectedArchetype = archetype
      }

emptySelections :: Selections Maybe f
emptySelections = Selections [] Nothing Nothing Nothing Nothing Nothing Nothing

makeLenses ''Selections

type Rule f = forall m. (MonadLogic m, MonadReader (Selections Maybe f) m) => m (Chart f)

{-| Find all charts that match the selections
-}
runRule :: Int -> Rule f -> Selections Maybe f -> [Chart f]
runRule n rule s = LogicT.observeMany n (runReaderT rule s)

archetype :: Chart f -> Archetype
archetype = \case
  Charts.VerticalBarChart{}   -> VerticalBarChart
  Charts.HorizontalBarChart{} -> HorizontalBarChart
  Charts.Scatterplot{}        -> Scatterplot
  Charts.LineChart{}          -> Linechart
  Charts.LineChart2Axis{}     -> Linechart
  Charts.Heatmap{}            -> Heatmap
