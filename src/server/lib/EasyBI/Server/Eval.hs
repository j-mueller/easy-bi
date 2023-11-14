{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-} -- needed beacuse of @makeSelect@
{-| Evaluate EasyBI queries against a database
-}
module EasyBI.Server.Eval
  ( DbBackend (..)
  , DbConnectionPool (..)
  , evalQuery
  , evalQueryDebug
    -- ** SQLite backend
  , evalQuerySQLite
  , withConnection
  , withDbConnectionPool
    -- * JSON rows
  , asJSONRowsPostgres
  , asJSONRowsSqlite
    -- * Restricting queries
  , SelectQuery (..)
  , applyFieldModifiers
  , fetchFirst
  , from
  , having
  , offset
  , setQuantifier
  , where_
  ) where

import Control.Exception              (bracket)
import Control.Lens                   (Prism', makeLenses, preview, prism',
                                       review, (%=), (&), (.~))
import Control.Monad.Except           (MonadError (..))
import Control.Monad.State            (execStateT)
import Data.Foldable                  (traverse_)
import Data.Map                       (Map)
import Data.Map                       qualified as Map
import Data.Pool                      (Pool)
import Data.Pool                      qualified as Pool
import Data.String                    (IsString (..))
import Database.SQLite.Simple         qualified as Sqlite
import EasyBI.Server.Visualisation    (AMeasurement (..), FieldInMode (..),
                                       InOut (..), MeasurementAndField (..),
                                       SortOrder (..), SqlFieldName (..),
                                       fromSqlName)
import EasyBI.Util.JSON               (WrappedObject (..))
import Language.SQL.SimpleSQL.Dialect qualified as Dialect
import Language.SQL.SimpleSQL.Pretty  qualified as Pretty
import Language.SQL.SimpleSQL.Syntax  (Alias (..), Direction (..),
                                       GroupingExpr (..), Name (..),
                                       NullsOrder (..), QueryExpr (..),
                                       ScalarExpr (App, BinOp, Iden, Star, StringLit),
                                       SetQuantifier, SortSpec (..),
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
  let open = Sqlite.open fp
              -- conn <-
              -- Sqlite.execute_ conn "PRAGMA mode=json"
              -- pure conn
      cfg =
        Pool.setNumStripes (Just 1)
        $ Pool.defaultPoolConfig
            open
            Sqlite.close
            60 -- 1 minute TTL
            1
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
evalQuery dbBackend = evalQueryDebug (\_ -> pure ()) dbBackend

{-| Evaluate a query, returning a list of JSON objects and using the given function to log the query output
-}
evalQueryDebug :: (String -> IO ()) -> DbConnectionPool -> QueryExpr -> IO [WrappedObject]
evalQueryDebug dbg dbBackend query =
  withConnection dbBackend
    (evalQuerySQLite dbg query)

{-| Evaluate a query pn a SQLite database, returning a list of JSON objects
-}
evalQuerySQLite :: (String -> IO ()) -> QueryExpr -> Sqlite.Connection -> IO [WrappedObject]
evalQuerySQLite dbg (asJSONRowsSqlite -> query) connection = do
  let qry = Pretty.prettyQueryExpr Dialect.postgres query
  dbg qry
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


data SelectQuery =
  SelectQuery
    { _setQuantifier :: SetQuantifier
    , _selectList_   :: [(ScalarExpr, Maybe Name)]
    , _from          :: [TableRef]
    , _where_        :: Maybe ScalarExpr
    , _groupBy       :: [GroupingExpr]
    , _having        :: Maybe ScalarExpr
    , _orderBy       :: [SortSpec]
    , _offset        :: Maybe ScalarExpr
    , _fetchFirst    :: Maybe ScalarExpr
    }

makeLenses ''SelectQuery

_Select :: Prism' QueryExpr SelectQuery
_Select = prism' from_ to_ where
  to_ Select{qeSetQuantifier, qeSelectList, qeFrom, qeWhere, qeGroupBy, qeHaving, qeOrderBy, qeOffset, qeFetchFirst} =
    Just
      SelectQuery
        { _setQuantifier = qeSetQuantifier
        , _selectList_   = qeSelectList
        , _from          = qeFrom
        , _where_        = qeWhere
        , _groupBy       = qeGroupBy
        , _having        = qeHaving
        , _orderBy       = qeOrderBy
        , _offset        = qeOffset
        , _fetchFirst    = qeFetchFirst
        }
  to_ _ = Nothing
  from_ SelectQuery{_setQuantifier, _selectList_, _from, _where_, _groupBy, _having, _orderBy, _offset, _fetchFirst} =
          Select
            { qeSetQuantifier = _setQuantifier
            , qeSelectList    = _selectList_
            , qeFrom          = _from
            , qeWhere         = _where_
            , qeGroupBy       = _groupBy
            , qeHaving        = _having
            , qeOrderBy       = _orderBy
            , qeOffset        = _offset
            , qeFetchFirst    = _fetchFirst
            }

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

{-| Build the query by inserting the given field definitions into the
original SQL query
-}
applyFieldModifiers :: MonadError QueryBuildError m => [FieldInMode In] -> QueryExpr -> m QueryExpr
applyFieldModifiers fields (preview _Select -> Just k) = do
  defs <- fieldDefinitions k
  fmap (review _Select) $ flip execStateT (k & selectList_ .~ mempty & groupBy .~ mempty) $ flip traverse fields $ \FieldInMode{sqlFieldName, sortOrder, fieldOptions=MeasurementAndField{mFieldType}} -> do
    (scalarExpr, fieldName) <- maybe (throwError $ FieldNotFound sqlFieldName) pure (Map.lookup sqlFieldName defs)
    selectList_ %= (:) (scalarExpr, Just fieldName)
    traverse_ (\o -> orderBy %= (:) (SortSpec scalarExpr o NullsOrderDefault)) (mapDir sortOrder)
    case mFieldType of
      AQuantitativeMeasurement -> pure ()
      _                        -> groupBy %= (:) (SimpleGroup scalarExpr)
applyFieldModifiers _ _ = throwError UnexpectedQueryType

mapDir :: SortOrder -> Maybe Direction
mapDir = \case
  Ascending  -> Just Asc
  Descending -> Just Desc
  None       -> Nothing

data QueryBuildError =
  FieldNotFound SqlFieldName
  | UnnamedFieldInCubeDefinition -- ^ Found an unnamed field
  | UnexpectedQueryType -- ^ Exepected select, found something else
    deriving Show

{-| The field definitions of the original SQL query that defines the cube
-}
fieldDefinitions :: MonadError QueryBuildError m => SelectQuery -> m (Map SqlFieldName (ScalarExpr, Name))
fieldDefinitions SelectQuery{_selectList_} = do
  let mkEntry (_, Nothing)   = throwError UnnamedFieldInCubeDefinition
      mkEntry (expr, Just n) = pure (fromSqlName n, (expr, n))
  Map.fromList <$> traverse mkEntry _selectList_
