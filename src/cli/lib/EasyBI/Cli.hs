{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-| CLI for easy-bi
-}
module EasyBI.Cli
  ( runCli
  ) where

import Control.Exception      (bracket)
import Control.Monad.IO.Class (MonadIO (..))
import EasyBI.Cli.Command     (Command (..), commandParser)
import EasyBI.MonadLog        (MonadLog, logInfoS, runMonadLogKatipT)
import Katip                  qualified as K
import Options.Applicative    (customExecParser, disambiguate, helper, idm,
                               info, prefs, showHelpOnEmpty, showHelpOnError)
import System.IO              (stdout)

runCli :: IO ()
runCli = do
  mainScribe <- K.mkHandleScribe (K.ColorLog True) stdout (K.permitItem K.InfoS) K.V2
  initLogEnv <- K.initLogEnv "easy-bi" "cli"
  let makeLogEnv = K.registerScribe "stdout-main" mainScribe K.defaultScribeSettings initLogEnv
  bracket makeLogEnv K.closeScribes $ \le -> K.runKatipContextT le () "main" $ runMonadLogKatipT $ do
    command <- liftIO (customExecParser
                          (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                          (info (helper <*> commandParser) idm))
    runCommand command

runCommand :: MonadLog m => Command -> m ()
runCommand = \case
  CheckSchema -> logInfoS "Checking schema"
