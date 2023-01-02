{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-| CLI for easy-bi
-}
module EasyBI.Cli
  ( runCli
  ) where

import Control.Exception              (bracket)
import Control.Monad.Except           (MonadError (..), runExceptT)
import Control.Monad.IO.Class         (MonadIO (..))
import Data.Foldable                  (traverse_)
import Data.Map.Strict                qualified as Map
import Data.Text                      qualified as Text
import Data.Text.IO                   qualified as Text.IO
import EasyBI.Cli.Command             (Command (..), TimestampColumn (..),
                                       commandParser)
import EasyBI.MonadLog                (MonadLog, logInfo, logInfoS, logWarnS,
                                       runMonadLogKatipT)
import EasyBI.Sql.Types               (SqlType (STDateTime), rowFromSchema)
import Katip                          qualified as K
import Language.SQL.SimpleSQL.Dialect qualified as Dialect
import Language.SQL.SimpleSQL.Parse   (ParseError (..))
import Language.SQL.SimpleSQL.Parse   qualified as Parse
import Language.SQL.SimpleSQL.Syntax  (Statement (..))
import Options.Applicative            (customExecParser, disambiguate, helper,
                                       idm, info, prefs, showHelpOnEmpty,
                                       showHelpOnError)
import System.Exit                    (exitFailure)
import System.IO                      (stdout)

runCli :: IO ()
runCli = do
  mainScribe <- K.mkHandleScribe (K.ColorLog True) stdout (K.permitItem K.InfoS) K.V2
  initLogEnv <- K.initLogEnv "easy-bi" "cli"
  let makeLogEnv = K.registerScribe "stdout-main" mainScribe K.defaultScribeSettings initLogEnv
  bracket makeLogEnv K.closeScribes $ \le -> K.runKatipContextT le () "main" $ runMonadLogKatipT $ do
    result <- runExceptT $ do
      command <- liftIO (customExecParser
                            (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
                            (info (helper <*> commandParser) idm))
      runCommand command
    case result of
      Left err -> case err of
        SqlParseError ParseError{peFormattedError} -> do
          logWarnS peFormattedError
          liftIO exitFailure
      Right _ -> pure ()

data AppError =
  SqlParseError Parse.ParseError

runCommand :: (MonadIO m, MonadLog m, MonadError AppError m) => Command -> m ()
runCommand = \case
  CheckSchema{sqlFile, timestampColumns} -> checkSchema sqlFile timestampColumns

checkSchema :: (MonadLog m, MonadIO m, MonadError AppError m) => FilePath -> [TimestampColumn] -> m ()
checkSchema fp timestampColumns = do
  logInfoS "Checking schema"
  let mkCol (TimestampColumn c) = (c, STDateTime)
      typeOverrides = Map.fromList $ fmap mkCol timestampColumns
  txt <- Text.unpack <$> liftIO (Text.IO.readFile fp)
  statements <- either (throwError . SqlParseError) pure (Parse.parseStatements Dialect.postgres fp Nothing txt)
  flip traverse_ statements $ \case
    CreateTable names elements -> do
      logInfoS ("Create table " <> show names)
      let tp = rowFromSchema elements typeOverrides
      logInfo tp
    _ -> logWarnS "Ignoring unexpected SQL statement"
