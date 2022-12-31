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
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}

module EasyBI.Vis.Types
  ( ColorChannel (..)
  , Encoding (..)
  , Mark (..)
  , MarkChannel (..)
  , Measurement (..)
  , OutOf (..)
  , PositionChannel (..)
  , Scale (..)
  , ScaleTp (..)
  , colorChannel
  , emptyColorChannel
  , emptyEncoding
  , emptyMarkChannel
  , emptyPosChannel
  , field
  , mark
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

data MarkChannel
  = MarkChannel
    { _markChannelMark :: Maybe Mark
    } deriving (Eq, Ord, Show)

emptyMarkChannel :: MarkChannel
emptyMarkChannel = MarkChannel Nothing

data Mark = Bar | Point | Line | Rect
  deriving (Eq, Ord, Show)

data PositionChannel f
  = PositionChannel
      { _positionChannelField :: Maybe f
      , _positionChannelTitle :: Maybe Text
      , _positionChannelScale :: Scale
      } deriving (Eq, Show, Functor, Foldable, Traversable)

emptyPosChannel :: PositionChannel f
emptyPosChannel = PositionChannel Nothing Nothing emptyScale

data ColorChannel f
  = ColorChannel
    { _colorChannelField :: Maybe f
    } deriving (Eq, Show, Functor, Foldable, Traversable)

emptyColorChannel :: ColorChannel f
emptyColorChannel = ColorChannel Nothing

{-| How many out of a maximum number
-}
data OutOf = OutOf Int Int
  deriving (Eq, Show)

-- | Specifies how a relation is displayed in graph
data Encoding f
  = Encoding
      { _positionX     :: PositionChannel f,
        _positionY     :: PositionChannel f,
        _colorChannel  :: ColorChannel f,
        _markChannel   :: MarkChannel,
        _wildCardsUsed :: Maybe OutOf
      } deriving (Eq, Show, Functor, Foldable, Traversable)

emptyEncoding :: Encoding f
emptyEncoding =
  Encoding
    { _positionX = emptyPosChannel
    , _positionY = emptyPosChannel
    , _colorChannel = emptyColorChannel
    , _markChannel = emptyMarkChannel
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
makeFields ''ColorChannel
makeFields ''MarkChannel
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
          Just a  -> setOrFail' (positionX . field) a *> pure [a]
  y' <- case s ^. yAxis of
          Nothing -> pure []
          Just a  -> setOrFail' (positionY . field) a *> pure [a]
  col <- case s ^. color of
          Nothing -> pure []
          Just c  -> setOrFail' (colorChannel . field) c *> pure [c]
  traverse_ (setOrFail' (markChannel . mark)) (s ^. selectedMark)
  pure $ nub $ wcs ++ x' ++ y' ++ col

{-
-}
runRule :: forall f. Eq f => Rule f -> Selections f -> [Encoding f]
runRule rule s =
  let rule' = selectedDimensions s >>= rule >>= guard . null
  in LogicT.observeAll (execStateT rule' emptyEncoding)

{-| Class of relations between fields (from the Mackinlay paper)

TODO: Do we really need this?
-}
class Relation a where
  measurement :: a -> Measurement
  dependsOn :: a -> a -> Bool -- ^ Functional dependencies
  fieldName :: a -> Text
