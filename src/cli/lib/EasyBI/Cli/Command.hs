module EasyBI.Cli.Command
  ( Command (..)
  , SchemaConfig (..)
  , TimestampColumn (..)
  , commandParser
  ) where

import Options.Applicative (Parser, command, fullDesc, help, info, long, many,
                            progDesc, strOption, subparser)

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
  deriving (Eq, Ord, Show)

commandParser :: Parser Command
commandParser =
  subparser $
    mconcat
      [ command "check-types" (info checkTypes (fullDesc <> progDesc "Read a file with SQL CREATE TABLE statements and check the schema"))
      ]

checkTypes :: Parser Command
checkTypes = CheckTypes <$> parseSchemaConfig
