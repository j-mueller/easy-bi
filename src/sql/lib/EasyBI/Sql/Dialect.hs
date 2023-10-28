{-| Custom dialect for sqlite
-}
module EasyBI.Sql.Dialect
  ( sqlite
  ) where

import Data.Set                       qualified as Set
import Language.SQL.SimpleSQL.Dialect (Dialect (..), postgres)

sqlite :: Dialect
sqlite =
  let dateTimeKeywords = Set.fromList ["date", "time"] in
  postgres
    { diKeywords           = filter (\x -> not $ x `Set.member` dateTimeKeywords) (diKeywords postgres)
    , diIdentifierKeywords = diIdentifierKeywords postgres ++ Set.toList dateTimeKeywords
    }
