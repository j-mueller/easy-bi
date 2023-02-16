{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-| Utilities for writing rules
-}
module EasyBI.Vis.Utils
  ( -- * Writing rules
    choose
  , chooseSubList
  , setOrFail'
  ) where

import Control.Applicative       (Alternative (..))
import Control.Lens              (Lens', (&), (.~), (^.))
import Control.Monad.Logic.Class (MonadLogic, (>>-))
import Control.Monad.State       (MonadState, get, put)

{-| Set a 'Maybe' field to a value. Fails if the field is already set to a different value.
-}
setOrFail :: forall a b g. (Eq b, Alternative g) => Lens' a (Maybe b) -> a -> b -> g a
setOrFail l a b =
  case a ^. l of
    Nothing           -> pure $ a & l .~ Just b
    Just b' | b == b' -> pure a
    _                 -> empty

{-| Variant of 'setOrFail' that operates on a 'MonadState'
-}
setOrFail' :: forall a b m. (MonadState a m, Alternative m, Eq b) => Lens' a (Maybe b) -> b -> m ()
setOrFail' l b = get @a >>= flip (setOrFail l) b >>= put

{-| Choose one item of the list
-}
choose :: Alternative g => [a] -> g (a, [a])
choose [] = empty
choose (x:xs) = go ([], x, xs) where
  go (p, cur, [])   = pure (cur, p)
  go (p, cur, y:ys) = pure (cur, p ++ (y:ys)) <|> go (cur:p, y, ys)

{-| Choose a sub-list of the input list
-}
chooseSubList :: (MonadLogic g) => [a] -> g ([a], [a])
chooseSubList r = go [] r where
  -- TODO: Can we make this more efficient?
  go current [] = pure (current, [])
  go current rest = pure (current, rest) <|> (choose rest >>- \(y, ys) -> go (y:current) ys)
