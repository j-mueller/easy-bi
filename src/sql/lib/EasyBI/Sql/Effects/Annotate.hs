{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
module EasyBI.Sql.Effects.Annotate(
  MonadAnnotate(..),
  AnnotateT(..),
  runAnnotateT
) where

import           Control.Monad.Except      (ExceptT, MonadError)
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Writer      (WriterT, runWriterT, tell)
import           EasyBI.Sql.Effects.Fresh  (MonadFresh)
import           EasyBI.Sql.Effects.Types  (Assumption, Constraint)

class Monad m => MonadAnnotate m where
  write :: ([Assumption], [Constraint]) -> m ()

newtype AnnotateT m a = AnnotateT{ unAnnotateT :: WriterT ([Assumption], [Constraint]) m a }
  deriving newtype (Functor, Applicative, Monad, MonadFresh, MonadTrans)

deriving newtype instance (MonadError e m) => MonadError e (AnnotateT m)

instance Monad m => MonadAnnotate (AnnotateT m) where
  write = AnnotateT . tell

instance MonadAnnotate m => MonadAnnotate (ExceptT e m) where
  write = lift . write

runAnnotateT :: AnnotateT m a -> m (a, ([Assumption], [Constraint]))
runAnnotateT = runWriterT . unAnnotateT
