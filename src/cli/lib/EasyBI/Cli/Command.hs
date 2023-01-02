module EasyBI.Cli.Command
  ( Command (..)
  , TimestampColumn (..)
  , commandParser
  ) where

import Options.Applicative (Parser, help, long, many, strOption)

newtype TimestampColumn = TimestampColumn{ unTimestampColumn :: String }
  deriving (Eq, Ord, Show)

data Command =
  CheckSchema
    { sqlFile          :: FilePath -- ^ file with SQL create command
    , timestampColumns :: [TimestampColumn] -- ^ Columns with timestamps (there is no TIMESTAMP type in Sqlite so we need to declare them manually)
    }
  deriving (Eq, Ord, Show)

commandParser :: Parser Command
commandParser =
  CheckSchema
    <$> strOption (long "sql-schema" <> help "File with SQL CREATE statement(s)")
    <*> many (TimestampColumn <$> strOption (long "timestamp" <> help "Columns with timestamp values"))
