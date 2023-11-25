{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-| All of the charts that we support
-}
module EasyBI.Vis.Charts
  ( Chart (..)
  , HeatmapSpec (..)
  , HorizontalBarChartSpec (..)
  , LineChartSpec (..)
  , LineChartSpec2Axis (..)
  , ScatterplotSpec (..)
  , VerticalBarChartSpec (..)
  ) where

import Codec.Serialise (Serialise (..))
import Data.Aeson      (FromJSON (..), ToJSON (..))
import GHC.Generics    (Generic)

data VerticalBarChartSpec f =
  VerticalBarChartSpec
    { vbcXAxis :: f
    , vbcYAxis :: f
    , vbcColor :: Maybe f
    }
    deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (ToJSON, FromJSON, Serialise)

data HorizontalBarChartSpec f =
  HorizontalBarChartSpec
    { hbcXAxis :: f
    , hbcYAxis :: f
    , hbcColor :: Maybe f
    }
    deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (ToJSON, FromJSON, Serialise)

data ScatterplotSpec f =
  ScatterplotSpec
    { spX     :: f
    , spY     :: f
    , spColor :: Maybe f
    }
    deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (ToJSON, FromJSON, Serialise)

data LineChartSpec f =
  LineChartSpec
    { lcX     :: f
    , lcY     :: f
    , lcColor :: Maybe f
    }
    deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (ToJSON, FromJSON, Serialise)

data LineChartSpec2Axis f =
  LineChartSpec2Axis
    { lc2X  :: f
    , lc2Y  :: f
    , lc2Y2 :: f
    }
    deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (ToJSON, FromJSON, Serialise)

data HeatmapSpec f =
  HeatmapSpec
    { hsX       :: f
    , hsY       :: f
    , hsMeasure :: f
    }
    deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (ToJSON, FromJSON, Serialise)

data Chart f =
  VerticalBarChart (VerticalBarChartSpec f)
  | HorizontalBarChart (HorizontalBarChartSpec f)
  | Scatterplot (ScatterplotSpec f)
  | LineChart (LineChartSpec f)
  | LineChart2Axis (LineChartSpec2Axis f)
  | Heatmap (HeatmapSpec f)
    deriving stock (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)
    deriving anyclass (ToJSON, FromJSON, Serialise)
