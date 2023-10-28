{-# LANGUAGE LambdaCase #-}
{-
-}
module EasyBI.Test.Utils
  ( checkProcessHasNotDied
  , createSystemTempDirectory
  , failure
  , withLogFile
  , withTempDir
    -- * Using the sample databases
  , SampleDB (..)
  , parseQuery
  , sampleCatalog
  , withSampleDb
  ) where

import Control.Exception             (catch, onException, throwIO)
import Control.Monad.IO.Class        (MonadIO (..))
import Data.ByteString               (ByteString)
import Data.ByteString               qualified as BS
import Data.Text                     (Text)
import Data.Text                     qualified as Text
import Data.Text.Encoding            qualified as Text
import Data.Void                     (Void)
import EasyBI.Sql.Catalog            (Catalog)
import EasyBI.Sql.Catalog            qualified as Catalog
import EasyBI.Sql.Dialect            qualified as Dialect
import GHC.IO.Exception              (IOErrorType (UnsatisfiedConstraints),
                                      ioe_type)
import GHC.Stack                     (HasCallStack, SrcLoc, callStack,
                                      getCallStack)
import Language.SQL.SimpleSQL.Parse  qualified as Parse
import Language.SQL.SimpleSQL.Syntax (QueryExpr)
import Paths_easy_bi_server          qualified as Pkg
import System.Directory              (createDirectoryIfMissing,
                                      removePathForcibly)
import System.Exit                   (ExitCode (..))
import System.FilePath               (takeDirectory, (</>))
import System.Info                   (os)
import System.IO                     (BufferMode (NoBuffering), Handle,
                                      IOMode (AppendMode), hSetBuffering,
                                      withFile)
import System.IO.Temp                (createTempDirectory,
                                      getCanonicalTemporaryDirectory)
import System.Process                (ProcessHandle, waitForProcess)
import Test.HUnit.Lang               (FailureReason (Reason),
                                      HUnitFailure (HUnitFailure))

-- | Open given log file non-buffered in append mode and print a message with
-- filepath to @stderr@ on exceptions.
withLogFile :: FilePath -> (Handle -> IO a) -> IO a
withLogFile filepath io = do
  createDirectoryIfMissing True (takeDirectory filepath)
  withFile filepath AppendMode (\out -> hSetBuffering out NoBuffering >> io out)
    `onException` putStrLn ("Logfile written to: " <> filepath)

-- | Create a unique temporary directory.
createSystemTempDirectory :: String -> IO FilePath
createSystemTempDirectory template = do
  tmpDir <- case os of
    "darwin" -> pure "/tmp" -- https://github.com/input-output-hk/hydra/issues/158.
    _        -> getCanonicalTemporaryDirectory
  createTempDirectory tmpDir template

-- | Create a temporary directory for the given 'action' to use.
-- The directory is removed if and only if the action completes successfuly.
withTempDir :: MonadIO m => String -> (FilePath -> m r) -> m r
withTempDir baseName action = do
  tmpDir <- liftIO $ createSystemTempDirectory baseName
  res <- action tmpDir
  liftIO $ cleanup 0 tmpDir
  pure res
 where
  -- NOTE: Somehow, since 1.35.0, cleaning-up cardano-node database directory
  -- _sometimes_ generates an empty 'clean' file which prevents the 'db' folder
  -- to be fully removed and triggers an 'UnsatisfiedConstraints' IOException.
  cleanup (maxAttempts :: Word) dir =
    removePathForcibly dir
      `catch` ( \e -> case ioe_type e of
                  UnsatisfiedConstraints ->
                    if maxAttempts < 3 then cleanup (succ maxAttempts) dir else throwIO e
                  _ ->
                    throwIO e
              )

-- | Wait for process termination and do 'failure' on non-zero exit code.
-- This function is useful for end-to-end testing of external processes esp. in
-- conjunction with 'race' combinator:
--
-- @@
-- withCreateProcess p $
--   \_stdin _stdout _stderr processHandle -> do
--       race_
--         (checkProcessHasNotDied "my-process" processHandle)
--         doStuff
-- @@
checkProcessHasNotDied :: Text -> ProcessHandle -> IO Void
checkProcessHasNotDied name processHandle =
  waitForProcess processHandle >>= \case
    ExitSuccess -> failure "Process has died"
    ExitFailure exit -> failure $ "Process " <> show name <> " exited with failure code: " <> show exit

-- | Fails a test with given error message.
-- This function improves over existing 'expectationFailure' by throwing a
-- 'HUnitFailure' exception containig the location of the error and providing
-- better callstack context.
failure :: HasCallStack => String -> IO a
failure msg =
  throwIO (HUnitFailure location $ Reason msg)

-- | Provides the source code location where this function is called.
-- This relies on the <https://hackage.haskell.org/package/base-4.15.0.0/docs/GHC-Exception.html#t:CallStack CallStack>
-- information provided by GHC and to be useful requires all functions to be properly
-- annotated.
location :: HasCallStack => Maybe SrcLoc
location = case reverse $ getCallStack callStack of
  (_, loc) : _ -> Just loc
  _            -> Nothing

data SampleDB =
  Sales -- ^ sales data (timestamp not properly formatted)
  | Outages -- ^ electricity outages

sampleDbSqliteFile :: SampleDB -> FilePath
sampleDbSqliteFile = \case
  Sales   -> "sales.db"
  Outages -> "outages.sqlite"

-- | Location of the test data
sampleDbPath :: SampleDB -> FilePath
sampleDbPath = \case
  Sales   -> "sales"
  Outages -> "outages"

sampleDbSchema :: SampleDB -> FilePath
sampleDbSchema = \case
  Sales   -> "schema.sql"
  Outages -> "schema.sql"

{-| Use the sqlite sample database
-}
withSampleDb :: FilePath -> SampleDB -> (FilePath -> IO ()) -> IO ()
withSampleDb workDir sampleDB action = do
  let destination = workDir </> sampleDbSqliteFile sampleDB
  readDataFile (sampleDbPath sampleDB </> sampleDbSqliteFile sampleDB) >>= BS.writeFile destination
  action destination

-- | Read a data file from the distribution
readDataFile :: FilePath -> IO ByteString
readDataFile source = do
  filename <- Pkg.getDataFileName ("test" </> "data" </> source)
  BS.readFile filename

parseQuery :: String -> IO QueryExpr
parseQuery str =
  case Parse.parseQueryExpr Dialect.sqlite "inline query" Nothing str of
    Left err -> failure (show err)
    Right x  -> pure x

sampleCatalog :: SampleDB -> IO Catalog
sampleCatalog db = do
  let loc = sampleDbPath db </> sampleDbSchema db
  file <- Text.unpack . Text.decodeUtf8 <$> readDataFile loc
  statements <- either (failure . show) pure (Parse.parseStatements Dialect.sqlite loc Nothing file)
  case Catalog.fromStatements mempty statements of
    Left err       -> failure (show err)
    Right (_, cat) -> pure cat
