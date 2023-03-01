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
  ( Encoding (..)
  , Mark (..)
  , Measurement (..)
  , PositionChannel (..)
  , Scale (..)
  , ScaleTp (..)
  , archetype
  , colorChannel
  , emptyEncoding
  , field
  , fieldPositionChannel
  , markChannel
  , positionX
  , positionY
  , scale
  , scaleTp
  , title
    -- * Relations
  , Relation (..)
    -- * Archetypes
  , Archetype (..)
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
    -- * Rules
  , Rule
  , runRule
    -- * Scoring
  , Score (..)
  , score
  ) where

import Codec.Serialise           (Serialise)
import Control.Lens              (makeFields, makeLenses, (^.))
import Control.Monad             (guard)
import Control.Monad.Logic       qualified as LogicT
import Control.Monad.Logic.Class (MonadLogic)
import Control.Monad.State       (MonadState, execStateT)
import Data.Aeson                (FromJSON, ToJSON)
import Data.Foldable             (traverse_)
import Data.List                 (nub)
import Data.Maybe                (isJust)
import Data.Semigroup            (Sum (..))
import Data.Text                 (Text)
import EasyBI.Vis.Utils          (chooseSubList, setOrFail')
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
data Measurement = Nominal | Ordinal | Quantitative | TemporalAbs | TemporalRel -- | Geofeature
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

{-| Scales available in HVega.
-}
data ScaleTp = SLinear | SLog | SPow | STime | SUtc | SQuantile | SOrdinal
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

data Scale =
  Scale
    { _scaleTp :: Maybe ScaleTp
    } deriving stock (Eq, Ord, Show, Generic)
      deriving anyclass (ToJSON, FromJSON, Serialise)

emptyScale :: Scale
emptyScale = Scale Nothing

data Mark = Bar | Point | Line | Rect
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

data PositionChannel f
  = PositionChannel
      { _positionChannelField :: f
      , _positionChannelTitle :: Text
      , _positionChannelScale :: Scale
      } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)
        deriving anyclass (ToJSON, FromJSON, Serialise)

fieldPositionChannel :: Relation f => f -> PositionChannel f
fieldPositionChannel f = PositionChannel f (fieldLabel f) emptyScale

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
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Serialise)

-- | Specifies how a relation is displayed in graph
data Encoding f
  = Encoding
      { _positionX    :: Maybe (PositionChannel f),
        _positionY    :: Maybe (PositionChannel f),
        _colorChannel :: Maybe f,
        _markChannel  :: Maybe Mark,
        _archetype    :: Maybe Archetype
      } deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance (ToJSON (PositionChannel f), ToJSON f) => ToJSON (Encoding f)
instance (FromJSON (PositionChannel f), FromJSON f) => FromJSON (Encoding f)
instance (Serialise f) => Serialise (Encoding f)

{-| Ranking of an encoding
-}
newtype Score = Score{unScore :: Double }
  deriving newtype (Num, Eq, Ord, ToJSON, FromJSON, Serialise)
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (Sum Double)

{-| How "good" (informative) is the encoding?
-}
score :: Encoding f -> Maybe Score
score Encoding{_positionX, _positionY, _colorChannel, _markChannel} = do
  let definedScore :: Maybe a -> Score
      definedScore = maybe 0 (const 1)

      defScores = definedScore _positionX <> definedScore _positionY <> definedScore _colorChannel <> definedScore _markChannel

  guard (isJust _positionX || isJust _positionY)
  pure defScores

emptyEncoding :: Encoding f
emptyEncoding =
  Encoding
    { _positionX = Nothing
    , _positionY = Nothing
    , _colorChannel = Nothing
    , _markChannel = Nothing
    , _archetype = Nothing
    }

-- | Data submitted by the user
data Selections k f =
  Selections
    { _WildCards         :: [f]
    , _XAxis             :: k f
    , _YAxis             :: k f
    , _Color             :: k f
    , _selectedMark      :: k Mark
    , _selectedArchetype :: k Archetype
    } deriving stock Generic

deriving instance Eq f => Eq (Selections [] f)
deriving instance Eq f => Eq (Selections Maybe f)

deriving instance (ToJSON f, ToJSON (k f), ToJSON (k Mark), ToJSON (k Archetype)) => ToJSON (Selections k f)
deriving instance (FromJSON f, FromJSON (k f), FromJSON (k Mark), FromJSON (k Archetype)) => FromJSON (Selections k f)

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
  color <- maybeList (_Color sel)
  mk <- maybeList (_selectedMark sel)
  archetype <- maybeList (_selectedArchetype sel)
  pure
    Selections
      { _WildCards = _WildCards sel
      , _XAxis = xAxis
      , _YAxis = yAxis
      , _Color = color
      , _selectedMark = mk
      , _selectedArchetype = archetype
      }

emptySelections :: Selections Maybe f
emptySelections = Selections [] Nothing Nothing Nothing Nothing Nothing

makeLenses ''Encoding
makeFields ''PositionChannel
makeLenses ''Scale
makeLenses ''Selections

{-| Rules for visualisations.
The input is a list of available dimensions (that have not been assigned to
a channel yet).
The rule then takes one or more of those dimensions, assigns them to channels,
and returns the remaining dimensions (if any).
The type parameter @f@ is the type of columns in the data source
-}
type Rule f = forall m. (MonadLogic m, MonadState (Encoding f) m) => [f] -> m [f]

{-| Initialise the state with selections from the user
-}
selectedDimensions :: forall m f. (Relation f, MonadLogic m, Eq f, MonadState (Encoding f) m) => Selections Maybe f -> m [f]
selectedDimensions s = do
  (wcs, _) <- chooseSubList 3 (s ^. wildCards)
  x' <- case s ^. xAxis of
    Nothing -> pure []
    Just a  -> setOrFail' positionX (fieldPositionChannel a) *> pure [a]
  y' <- case s ^. yAxis of
    Nothing -> pure []
    Just a  -> setOrFail' positionY (fieldPositionChannel a) *> pure [a]
  col <- case s ^. color of
    Nothing -> pure []
    Just c  -> setOrFail' colorChannel c *> pure [c]
  traverse_ (setOrFail' markChannel) (s ^. selectedMark)
  traverse_ (setOrFail' archetype) (s ^. selectedArchetype)
  pure $ nub $ wcs ++ x' ++ y' ++ col

{-
-}
runRule :: forall f. (Relation f, Eq f) => Int -> Rule f -> Selections Maybe f -> [Encoding f]
runRule n rule s =
  let rule' = selectedDimensions s >>= rule >>= guard . null
  in LogicT.observeMany n (execStateT rule' emptyEncoding)
