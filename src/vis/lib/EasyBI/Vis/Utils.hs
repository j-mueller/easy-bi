{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-| Utilities for writing rules
-}
module EasyBI.Vis.Utils
  ( -- * Writing rules
    choose
  , choose1
  , choose2
  , choose3
  , chooseSubList
  , setOrFail'
  ) where

import Control.Applicative       (Alternative (..))
import Control.Lens              (Lens', (&), (.~), (^.))
import Control.Monad             (guard)
import Control.Monad.Logic.Class (MonadLogic, (>>-))
import Control.Monad.State       (MonadState, get, put)
import Data.List                 (partition)

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
chooseSubList :: (MonadLogic g) => Int -> [a] -> g ([a], [a])
chooseSubList maxLength r = go [] r where
  go current [] = pure (current, [])
  go current rest = pure (current, rest) <|> (guard (length current < maxLength) >> choose rest >>- \(y, ys) -> go (y:current) ys)

choose1 :: MonadLogic m => (f -> Bool) -> [f] -> m (f, [f])
choose1 p dims = do
  let (available, rest) = partition p dims
  choose available >>- \(x, rest') -> pure (x, rest' ++ rest)

choose2 :: MonadLogic m => (f -> Bool, f -> Bool) -> [f] -> m ((f, f), [f])
choose2 (pred1, pred2) dims = do
  choose1 pred1 dims >>- \(x1, rest) ->
    choose1 pred2 rest >>- \(x2, rest') ->
      pure ((x1, x2), rest')

choose3 :: MonadLogic m => (f -> Bool, f -> Bool, f -> Bool) -> [f] -> m ((f, f, f), [f])
choose3 (pred1, pred2, pred3) dims = do
  choose2 (pred1, pred2) dims >>- \((x1, x2), rest) ->
    choose1 pred3 rest >>- \(x3, rest') ->
      pure ((x1, x2, x3), rest')
