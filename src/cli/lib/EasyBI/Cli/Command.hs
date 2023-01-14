module EasyBI.Cli.Command
  ( Command (..)
  , SchemaConfig (..)
  , TimestampColumn (..)
  , commandParser
  ) where

import EasyBI.Server.Impl  (ServerConfig (..))
import Options.Applicative (Parser, auto, command, fullDesc, help, info, long,
                            many, option, progDesc, strOption, subparser, value)

newtype TimestampColumn = TimestampColumn{ unTimestampColumn :: String }
  deriving (Eq, Ord, Show)

data SchemaConfig =
  SchemaConfig
    { scSqlFile          :: FilePath
    , scTimestampColumns :: [TimestampColumn]
    }
    deriving (Eq, Ord, Show)

parseSchemaConfig :: Parser SchemaConfig
parseSchemaConfig =
  SchemaConfig
    <$> strOption (long "sql-schema" <> help "File with SQL CREATE statement(s)")
    <*> many (TimestampColumn <$> strOption (long "timestamp" <> help "Columns with timestamp values"))

data Command =
  CheckTypes SchemaConfig
  | StartServer SchemaConfig ServerConfig
  deriving (Eq, Ord, Show)

commandParser :: Parser Command
commandParser =
  subparser $
    mconcat
      [ command "check-types" (info checkTypes (fullDesc <> progDesc "Read a file with SQL CREATE TABLE statements and check the schema"))
      , command "start-server" (info startServer (fullDesc <> progDesc "Start the EasyBI server using the provided schema"))
      ]

checkTypes :: Parser Command
checkTypes = CheckTypes <$> parseSchemaConfig

startServer :: Parser Command
startServer = StartServer <$> parseSchemaConfig <*> parseServerConfig

parseServerConfig :: Parser ServerConfig
parseServerConfig =
  ServerConfig
    <$> option auto (long "port" <> value 8080 <> help "Server port")
