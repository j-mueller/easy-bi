module EasyBI.Cli.Command
  ( Command (..)
  , commandParser
  ) where

import Options.Applicative (Parser)

data Command =
  CheckSchema
  deriving (Eq, Ord, Show)

commandParser :: Parser Command
commandParser = pure CheckSchema
