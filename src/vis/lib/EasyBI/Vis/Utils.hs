{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-| Utilities for writing rules
-}
module EasyBI.Vis.Utils
  ( -- * Writing rules
    choose
  , choose1
  ) where

import Control.Applicative       (Alternative (..))
import Control.Monad.Logic.Class (MonadLogic, (>>-))
import Data.List                 (partition)

{-| Choose one item of the list. Returns the item, a list with items before the item, and a list with items after the item.
-}
choose :: Alternative g => [a] -> g (a, ([a], [a]))
choose [] = empty
choose (x:xs) = go ([], x, xs) where
  go (p, cur, [])   = pure (cur, (p, []))
  go (p, cur, y:ys) = pure (cur, (p, (y:ys))) <|> go (cur:p, y, ys)

choose1 :: MonadLogic m => (f -> Bool) -> [f] -> m (f, ([f], [f]))
choose1 p dims = do
  let (available, rest) = partition p dims
  choose available >>- \(x, (k, rest')) -> pure (x, (k, rest' ++ rest))
