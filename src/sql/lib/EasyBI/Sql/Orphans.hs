
{-# OPTIONS_GHC -Wno-orphans    #-}
module EasyBI.Sql.Orphans
  (
  ) where

import Language.SQL.SimpleSQL.Syntax (Name (..))

deriving instance Ord Name
