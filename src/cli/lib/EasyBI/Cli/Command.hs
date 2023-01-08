module EasyBI.Cli.Command
  ( Command (..)
  , TimestampColumn (..)
  , SchemaConfig(..)
  , commandParser
  ) where

import           Options.Applicative (Parser, command, fullDesc, help, info,
                                      long, many, progDesc, strOption,
                                      subparser)

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
  CheckSchema SchemaConfig
  | CheckTypes SchemaConfig FilePath
  deriving (Eq, Ord, Show)

commandParser :: Parser Command
commandParser =
  subparser $
    mconcat
      [ command "check-schema" (info checkSchema (fullDesc <> progDesc "Read a file with SQL CREATE TABLE statements and check the schema"))
      , command "check-types"  (info checkTypes  (fullDesc <> progDesc "Read a file with SQL SELECT statements and check their types"))
      ]

checkTypes :: Parser Command
checkTypes =
  CheckTypes
    <$> parseSchemaConfig
    <*> strOption (long "sql-query" <> help "File with SQL SELECT statement(s)")

checkSchema :: Parser Command
checkSchema =
  CheckSchema
    <$> parseSchemaConfig
