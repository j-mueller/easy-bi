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

module EasyBI.Vis.Types
  ( Encoding (..)
  , Mark (..)
  , Measurement (..)
  , OutOf (..)
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
  , wildCardsUsed
    -- * Relations
  , Relation (..)
    -- * Archetypes
  , Archetype (..)
    -- * User selections
  , Selections (..)
  , color
  , emptySelections
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
import Control.Applicative       (Alternative (..))
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

{-| Things that can be measured
-}
data Measurement = Nominal | Ordinal | Quantitative | TemporalAbs | TemporalRel -- | Geofeature
  deriving (Eq, Show)

{-| Scales available in HVega.
-}
data ScaleTp = SLinear | SLog | SPow | STime | SUtc | SQuantile | SOrdinal
  deriving (Eq, Show)

data Scale =
  Scale
    { _scaleTp :: Maybe ScaleTp
    } deriving (Eq, Show)

emptyScale :: Scale
emptyScale = Scale Nothing

data Mark = Bar | Point | Line | Rect
  deriving (Eq, Ord, Show)

data PositionChannel f
  = PositionChannel
      { _positionChannelField :: f
      , _positionChannelTitle :: Text
      , _positionChannelScale :: Scale
      } deriving (Eq, Show, Functor, Foldable, Traversable)

fieldPositionChannel :: f -> PositionChannel f
fieldPositionChannel f = PositionChannel f "" emptyScale

{-| How many out of a maximum number
-}
data OutOf = OutOf Int Int
  deriving (Eq, Show)

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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Specifies how a relation is displayed in graph
data Encoding f
  = Encoding
      { _positionX     :: Maybe (PositionChannel f),
        _positionY     :: Maybe (PositionChannel f),
        _colorChannel  :: Maybe f,
        _markChannel   :: Maybe Mark,
        _wildCardsUsed :: Maybe OutOf,
        _archetype     :: Maybe Archetype
      } deriving (Eq, Show, Functor, Foldable, Traversable)

{-| Ranking of an encoding
-}
newtype Score = Score{unScore :: Double }
  deriving newtype (Num, Eq, Ord, ToJSON, FromJSON, Serialise)
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (Sum Double)

{-| How "good" (informative) is the encoding?
-}
score :: Encoding f -> Maybe Score
score Encoding{_positionX, _positionY, _colorChannel, _markChannel, _wildCardsUsed} = do
  let definedScore :: Maybe a -> Score
      definedScore = maybe 0 (const 1)

      outOfScore :: OutOf -> Score
      outOfScore (OutOf _ 0) = 0
      outOfScore (OutOf a b) = 2 * Score (fromIntegral a / fromIntegral b)

      defScores = definedScore _positionX <> definedScore _positionY <> definedScore _colorChannel <> definedScore _markChannel

  guard (isJust _positionX || isJust _positionY)
  pure (defScores <> maybe 0 outOfScore _wildCardsUsed)

emptyEncoding :: Encoding f
emptyEncoding =
  Encoding
    { _positionX = Nothing
    , _positionY = Nothing
    , _colorChannel = Nothing
    , _markChannel = Nothing
    , _wildCardsUsed = Nothing
    , _archetype = Nothing
    }

-- | Data submitted by the user
data Selections f =
  Selections
    { _WildCards    :: [f]
    , _XAxis        :: Maybe f
    , _YAxis        :: Maybe f
    , _Color        :: Maybe f
    , _selectedMark :: Maybe Mark
    } deriving (Eq, Show)

emptySelections :: Selections f
emptySelections = Selections [] Nothing Nothing Nothing Nothing

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
selectedDimensions :: forall m f. (Alternative m, Eq f, MonadState (Encoding f) m) => Selections f -> m [f]
selectedDimensions s = do
  (wcs, _) <- chooseSubList (s ^. wildCards)
  setOrFail' wildCardsUsed (length wcs `OutOf` length (s ^. wildCards))
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
  pure $ nub $ wcs ++ x' ++ y' ++ col

{-
-}
runRule :: forall f. Eq f => Int -> Rule f -> Selections f -> [Encoding f]
runRule n rule s =
  let rule' = selectedDimensions s >>= rule >>= guard . null
  in LogicT.observeMany n (execStateT rule' emptyEncoding)

{-| Class of relations between fields (from the Mackinlay paper)
-}
class Relation a where
  measurement :: a -> Measurement
  fieldName :: a -> Text
