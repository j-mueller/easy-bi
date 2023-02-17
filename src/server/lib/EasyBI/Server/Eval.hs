{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-} -- needed beacuse of @makeSelect@
{-| Evaluate EasyBI queries against a database
-}
module EasyBI.Server.Eval
  ( DbBackend (..)
  , DbConnectionPool (..)
  , evalQuery
  , evalQuerySQLite
  , withConnection
  , withDbConnectionPool
    -- * JSON rows
  , asJSONRowsPostgres
  , asJSONRowsSqlite
    -- * Restricting queries
  , restrictTo
  ) where

import Control.Exception              (bracket)
import Data.Pool                      (Pool, PoolConfig (..))
import Data.Pool                      qualified as Pool
import Data.Set                       qualified as Set
import Data.String                    (IsString (..))
import Database.SQLite.Simple         qualified as Sqlite
import EasyBI.Util.JSON               (WrappedObject (..))
import Language.SQL.SimpleSQL.Dialect qualified as Dialect
import Language.SQL.SimpleSQL.Pretty  qualified as Pretty
import Language.SQL.SimpleSQL.Syntax  (Alias (..), GroupingExpr (..), Name (..),
                                       QueryExpr (..), ScalarExpr (..),
                                       TableRef (..), makeSelect)

data DbBackend
  = SqliteBackend{ sqliteFile :: FilePath }
  deriving (Eq, Show)
  -- TODO: Postgres

data DbConnectionPool
  = SqlitePool FilePath (Pool Sqlite.Connection)

{-| Take a database connection and do something with it
-}
withConnection :: DbConnectionPool -> (Sqlite.Connection -> IO a) -> IO a
withConnection (SqlitePool _ pool) = Pool.withResource pool

{-| Establish a connection to the database
-}
connectToDb :: DbBackend -> IO DbConnectionPool
connectToDb = \case
  SqliteBackend{sqliteFile} -> do
    sqlitePool <- newSqliteConnectionPool sqliteFile
    pure (SqlitePool sqliteFile sqlitePool)

{-| Create a new connection pool to a sqlite database
-}
newSqliteConnectionPool :: FilePath -> IO (Pool Sqlite.Connection)
newSqliteConnectionPool fp =
  let cfg =
        PoolConfig
          { createResource = do
              conn <- Sqlite.open fp
              -- Sqlite.execute_ conn "PRAGMA mode=json"
              pure conn
          , freeResource = Sqlite.close
          , poolCacheTTL = 60 -- 1 minute TTL
          , poolMaxResources = 1
          }
  in Pool.newPool cfg

cleanup :: DbConnectionPool -> IO ()
cleanup = \case
  SqlitePool _ pool -> Pool.destroyAllResources pool

{-| Connect to the database and perform any required cleaning afterwards
-}
withDbConnectionPool :: DbBackend -> (DbConnectionPool -> IO ()) -> IO ()
withDbConnectionPool backend action =
  bracket
    (connectToDb backend)
    action
    cleanup

{-| Evaluate a query, returning a list of JSON objects
-}
evalQuery :: DbConnectionPool -> QueryExpr -> IO [WrappedObject]
evalQuery dbBackend query =
  withConnection dbBackend
    (evalQuerySQLite query)

{-| Evaluate a query pn a SQLite database, returning a list of JSON objects
-}
evalQuerySQLite :: QueryExpr -> Sqlite.Connection -> IO [WrappedObject]
evalQuerySQLite (asJSONRowsSqlite -> query) connection = do
  let qry = Pretty.prettyQueryExpr Dialect.postgres query
  fmap Sqlite.fromOnly <$> Sqlite.query_ connection (fromString qry)

{-| Modify the query to return a list of JSON objects. Uses the sqlite-specific @json_object@
operator, which requires SQLite >= 3.38.0 or SQLite compiled with the SQLITE_ENABLE_JSON1 flag.
This function also assumes that each column returned by the SELECT statement is assigned a name.
Columns without a name are dropped.
-}
asJSONRowsSqlite :: QueryExpr -> QueryExpr
asJSONRowsSqlite expr =
  let mkSelect (Just n@(Name _ nm)) = [StringLit "'" "'" nm, Iden [n]]
      mkSelect _                    = []
      selections = fmap (mkSelect . snd) (selectList expr)
      op = App [Name Nothing "json_object"] (mconcat selections)
  in makeSelect
      { qeSelectList = [(op, Nothing)]
      , qeFrom       = [TRQueryExpr expr]
      }

selectList :: QueryExpr -> [(ScalarExpr, Maybe Name)]
selectList = \case
  Select{qeSelectList}    -> qeSelectList
  QueryExprSetOp{qe0}     -> selectList qe0
  With{qeQueryExpression} -> selectList qeQueryExpression
  QEComment _ e           -> selectList e
  _                       -> []


{-| Modify the query to return a list of JSON objects. Uses the postgres-specific
@row_to_json@ operator.
-}
asJSONRowsPostgres :: QueryExpr -> QueryExpr
asJSONRowsPostgres e =
  let nm = Name Nothing "values"
      rtj = Name Nothing "row_to_json"
      dot = BinOp (Iden [nm]) [Name Nothing "."] Star
  in With
      { qeWithRecursive = False
      , qeViews = [(Alias nm Nothing, e)]
      , qeQueryExpression =
          makeSelect
            { qeFrom = [TRSimple [nm]]
            , qeSelectList = [(App [rtj] [dot], Nothing)]
            }
      }

{-| Restrict the SELECT and GROUP bits of the query to
the fields in the given list
-}
restrictTo :: MonadFail m => [String] -> QueryExpr -> m QueryExpr
restrictTo (Set.fromList -> fields) = \case
  x@Select{qeSelectList=allSelects, qeGroupBy=allGroups} ->
    let hasName (Just (Name _ n)) = Set.member n fields
        hasName _                 = False
        isGroup (SimpleGroup  (Iden [Name _ n])) = Set.member n fields
        isGroup _                                = False
        newSelects = filter (hasName . snd) allSelects
        newGroups  = filter isGroup allGroups
    in pure x{qeSelectList = newSelects, qeGroupBy = newGroups}
  _ -> fail "Unsupported query"
