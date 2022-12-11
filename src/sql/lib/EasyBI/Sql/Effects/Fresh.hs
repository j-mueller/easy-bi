{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-| Typing SQL statements. Based on "Generalizing Hindley-Milner Type Inference Algorithms" by B. Heeren, J. Hage and D. Swierstra (technical report)
-}
module EasyBI.Sql.Effects.Fresh(
  MonadFresh(..),
  FreshT(..),
  runFreshT,
  evalFreshT,
  instantiate
  ) where

import           Control.Monad.Except       (ExceptT, MonadError (..))
import           Control.Monad.State.Strict (MonadState (..), StateT,
                                             evalStateT, runStateT)
import           Control.Monad.Trans.Class  (MonadTrans (..))
import           Control.Monad.Writer       (WriterT)
import           EasyBI.Sql.Effects.Types   (SqlType (..), TyScheme (..),
                                             TyVar (..), fromList, apply)

class Monad m => MonadFresh m where
  freshVar :: m TyVar

newtype FreshT m a = FreshT{ unFreshT :: StateT TyVarState m a }
  deriving newtype (Functor, Applicative, Monad, MonadTrans)

deriving newtype instance (MonadError e m) => MonadError e (FreshT m)

instance (MonadFresh m, Monoid s) => MonadFresh (WriterT s m) where
  freshVar = lift freshVar

instance (MonadFresh m) => MonadFresh (StateT s m) where
  freshVar = lift freshVar

instance MonadFresh m => MonadFresh (ExceptT e m) where
  freshVar = lift freshVar

runFreshT :: FreshT m a -> m (a, TyVarState)
runFreshT = flip runStateT (TyVarState 0) . unFreshT

evalFreshT :: Monad m => FreshT m a -> m a
evalFreshT = flip evalStateT (TyVarState 0) . unFreshT

instance Monad m => MonadFresh (FreshT m) where
  freshVar = FreshT $ do
    TyVarState i <- get
    put (TyVarState $ succ i)
    return (TyVar i)

newtype TyVarState = TyVarState Int
  deriving stock (Eq, Ord, Show)

{-| Assign fresh type variables to all quantified type variables
-}
instantiate :: MonadFresh m => TyScheme TyVar (SqlType TyVar) -> m (SqlType TyVar)
instantiate (TyScheme vars tp) = do
  let f tv = (tv,) . STVar <$> freshVar
  vars' <- traverse f vars
  let subs = fromList vars'
  pure (apply subs tp)
