{-# LANGUAGE DerivingStrategies #-}
{-| Simple logging
-}
module EasyBI.Util.MonadLog
  ( MonadLog (..)
  , MonadLogIgnoreT (..)
  , MonadLogKatipT (..)
  , logInfo
  , logInfoS
  , logWarn
  , logWarnS
    -- * Etc.
  , logUnless
  ) where

import Control.Monad              (unless)
import Control.Monad.Catch        (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Reader       (ReaderT (..), lift)
import Control.Monad.State        (StateT (..))
import Control.Monad.State.Strict qualified as State.Strict
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Maybe  (MaybeT (..))
import Data.String                (IsString (..))
import Data.Void                  (Void)
import Katip                      (KatipContext, Severity (..))
import Katip qualified
import Prettyprinter              (Doc, Pretty (..), defaultLayoutOptions,
                                   layoutPretty)
import Prettyprinter.Render.Text  qualified as Render

class Monad m => MonadLog m where
  logInfo' :: Doc Void -> m ()
  logWarn' :: Doc Void -> m ()

instance MonadLog m => MonadLog (ReaderT e m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'

instance MonadLog m => MonadLog (StateT s m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'

instance MonadLog m => MonadLog (MaybeT m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'

instance MonadLog m => MonadLog (State.Strict.StateT s m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'

instance MonadLog m => MonadLog (ExceptT e m) where
  logInfo' = lift . logInfo'
  logWarn' = lift . logWarn'

logInfo :: forall a m. (Pretty a, MonadLog m) => a -> m ()
logInfo = logInfo' . pretty

logInfoS :: forall  m. (MonadLog m) => String -> m ()
logInfoS = logInfo' . fromString

logWarn :: forall a m. (Pretty a, MonadLog m) => a -> m ()
logWarn = logWarn' . pretty

logWarnS :: forall m. MonadLog m => String -> m ()
logWarnS = logWarn' . fromString

newtype MonadLogIgnoreT m a = MonadLogIgnoreT { runMonadLogIgnoreT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadMask)

instance Monad m => MonadLog (MonadLogIgnoreT m) where
  logInfo' _ = pure ()
  logWarn' _ = pure ()

newtype MonadLogKatipT m a = MonadLogKatipT { runMonadLogKatipT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadCatch, MonadThrow, MonadMask)

instance KatipContext m => MonadLog (MonadLogKatipT m) where
    logInfo' s =
        let mkStr = Katip.logStr . Render.renderLazy . layoutPretty defaultLayoutOptions
        in MonadLogKatipT (Katip.logFM InfoS (mkStr s))
    logWarn' s =
        let mkStr = Katip.logStr . Render.renderLazy . layoutPretty defaultLayoutOptions
        in MonadLogKatipT (Katip.logFM WarningS (mkStr s))

logUnless :: MonadLog m => Bool -> String -> m ()
logUnless w m = unless w (logInfoS m)
