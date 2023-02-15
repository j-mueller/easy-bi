{-# LANGUAGE NamedFieldPuns #-}
{-| Server state
-}
module EasyBI.Server.State
  ( ServerState (..)
  , emptyState
  , findQuery
  , findView
  , stateFromList
  , views
  ) where

import Data.List                     (intercalate)
import Data.Map.Strict               (Map)
import Data.Map.Strict               qualified as Map
import EasyBI.Server.View            (View (..), hashView, queryExpr)
import EasyBI.Sql.Catalog            (Catalog (..), TypedQueryExpr)
import EasyBI.Util.NiceHash          (Hashed, NiceHash, Plain, hNiceHash,
                                      hPlain, niceHash)
import Language.SQL.SimpleSQL.Syntax (Name (..))

data ServerState =
  ServerState
    { ssViews   :: Map (NiceHash (View Hashed)) (View Hashed)
    , ssQueries :: Map (NiceHash TypedQueryExpr) TypedQueryExpr
    }

{-| Empty server state
-}
emptyState :: ServerState
emptyState = ServerState mempty mempty

views :: Catalog -> [View Plain]
views =
  let mkView (names, typedQueryExpr) =
        View
          { vQuery = hPlain typedQueryExpr
          , vTitle = mkTitle names
          }

      mkTitle [] = "<no title>"
      mkTitle xs =
        let get (Name _ n) = n
        in intercalate "." (get <$> xs)
  in fmap mkView . Map.toList . _views

stateFromList :: [View Plain] -> ServerState
stateFromList views_ =
  let mkV v =
        let v' = hashView v
        in (niceHash v', v', queryExpr v)
      viewsH = mkV <$> views_

  in ServerState
      { ssViews = Map.fromList (fmap (\(a, b, _) -> (a, b)) viewsH)
      , ssQueries = Map.fromList $ fmap (\(_, a, c) -> (hNiceHash $ vQuery a, c)) viewsH
      }

-- | Lookup the query definition
findQuery :: ServerState -> NiceHash TypedQueryExpr -> Maybe TypedQueryExpr
findQuery ServerState{ssQueries} h = Map.lookup h ssQueries

findView :: ServerState -> NiceHash (View Hashed) -> Maybe (View Hashed)
findView ServerState{ssViews} h = Map.lookup h ssViews
