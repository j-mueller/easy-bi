{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
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

import Control.Applicative       (Alternative (..))
import Control.Lens              (makeFields, makeLenses, (^.))
import Control.Monad             (guard)
import Control.Monad.Logic       qualified as LogicT
import Control.Monad.Logic.Class (MonadLogic)
import Control.Monad.State       (MonadState, execStateT)
import Data.Foldable             (traverse_)
import Data.List                 (nub)
import Data.Text                 (Text)
import EasyBI.Vis.Utils          (chooseSubList, setOrFail')

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

-- | Specifies how a relation is displayed in graph
data Encoding f
  = Encoding
      { _positionX     :: Maybe (PositionChannel f),
        _positionY     :: Maybe (PositionChannel f),
        _colorChannel  :: Maybe f,
        _markChannel   :: Maybe Mark,
        _wildCardsUsed :: Maybe OutOf
      } deriving (Eq, Show, Functor, Foldable, Traversable)

newtype Score = Score{unScore :: Double }

{-| How "good" (informative) is the encoding?
-}
score :: Encoding f -> Maybe Score
score _ = Nothing

emptyEncoding :: Encoding f
emptyEncoding =
  Encoding
    { _positionX = Nothing
    , _positionY = Nothing
    , _colorChannel = Nothing
    , _markChannel = Nothing
    , _wildCardsUsed = Nothing
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
runRule :: forall f. Eq f => Rule f -> Selections f -> [Encoding f]
runRule rule s =
  let rule' = selectedDimensions s >>= rule >>= guard . null
  in LogicT.observeAll (execStateT rule' emptyEncoding)

{-| Class of relations between fields (from the Mackinlay paper)
-}
class Relation a where
  measurement :: a -> Measurement
  fieldName :: a -> Text
