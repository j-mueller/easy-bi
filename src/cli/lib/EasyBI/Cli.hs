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
import Control.Lens                   (at, use, (.=))
import Control.Monad.Except           (MonadError (..), runExceptT)
import Control.Monad.IO.Class         (MonadIO (..))
import Control.Monad.State.Strict     (evalStateT)
import Data.Foldable                  (traverse_)
import Data.Map.Strict                qualified as Map
import Data.Text                      qualified as Text
import Data.Text.IO                   qualified as Text.IO
import EasyBI.Cli.Command             (Command (..), SchemaConfig (..),
                                       TimestampColumn (..), commandParser)
import EasyBI.Sql.BuiltinTypes        (defaultTypeEnv)
import EasyBI.Sql.Catalog             (TypedQueryExpr (..), tables, views)
import EasyBI.Sql.Class               (render, runInferType)
import EasyBI.Sql.Types               (SqlType (STDateTime),
                                       SqlVar (AnIdentifier), TypeEnv (..),
                                       rowFromSchema)
import EasyBI.Util.MonadLog           (MonadLog, logInfo', logInfoS, logWarn,
                                       logWarnS, runMonadLogKatipT)
import Katip                          qualified as K
import Language.SQL.SimpleSQL.Dialect qualified as Dialect
import Language.SQL.SimpleSQL.Parse   (ParseError (..))
import Language.SQL.SimpleSQL.Parse   qualified as Parse
import Language.SQL.SimpleSQL.Syntax  (Statement (..))
import Options.Applicative            (customExecParser, disambiguate, helper,
                                       idm, info, prefs, showHelpOnEmpty,
                                       showHelpOnError)
import Prettyprinter                  (pretty, (<+>))
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
  CheckTypes SchemaConfig{scSqlFile, scTimestampColumns} -> checkSchema scSqlFile scTimestampColumns
  StartServer _schemaConfig _serverConfig -> do
    logWarnS "start-server: not implemented"
    pure ()

checkSchema :: (MonadLog m, MonadIO m, MonadError AppError m) => FilePath -> [TimestampColumn] -> m ()
checkSchema fp timestampColumns = do
  logInfoS "Checking schema"
  let mkCol (TimestampColumn c) = (c, STDateTime)
      typeOverrides = Map.fromList $ fmap mkCol timestampColumns
  txt <- Text.unpack <$> liftIO (Text.IO.readFile fp)
  statements <- either (throwError . SqlParseError) pure (Parse.parseStatements Dialect.postgres fp Nothing txt)
  flip evalStateT mempty $ flip traverse_ statements $ \case
    CreateTable names elements -> do
      logInfoS ("Create table " <> show names)
      let tp = rowFromSchema elements typeOverrides
      tables . at (AnIdentifier names) .= Just tp
    CreateView _ names _ queryExpr _ -> do
      logInfoS ("Create view " <> show names)
      tyEnv <- TypeEnv <$> use tables
      case runInferType (tyEnv <> defaultTypeEnv) queryExpr of
        Left err -> do
          logWarnS $ "Type inference failed for '" <> render Dialect.postgres queryExpr <> "'"
          logWarn err
        Right (_, tp, _) -> do
          logInfo' $ "Inferred type:" <+> pretty tp
          views . at names .= Just (TypedQueryExpr queryExpr tp)
    _ -> logWarnS "Ignoring unexpected SQL statement"
