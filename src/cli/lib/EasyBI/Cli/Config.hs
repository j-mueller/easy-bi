module EasyBI.Cli.Config
  ( Db (..)
  ) where

data Db =
  Sqlite FilePath
  deriving (Eq, Ord, Show)
