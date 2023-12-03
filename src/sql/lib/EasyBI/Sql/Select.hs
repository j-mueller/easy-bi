{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-| Type for select queries
-}
module EasyBI.Sql.Select
  ( SelectQuery (..)
  , _Select
  , fetchFirst
  , from
  , groupBy
  , having
  , offset
  , orderBy
  , selectList_
  , setQuantifier
  , where_
  ) where

import Codec.Serialise               (Serialise)
import Control.Lens                  (Prism', makeLenses, prism')
import Data.Aeson                    (FromJSON, ToJSON)
import EasyBI.Sql.Orphans            ()
import GHC.Generics                  (Generic)
import Language.SQL.SimpleSQL.Syntax (GroupingExpr (..), Name (..),
                                      QueryExpr (..), ScalarExpr, SetQuantifier,
                                      SortSpec (..), TableRef (..))

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
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Serialise, ToJSON, FromJSON)

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
