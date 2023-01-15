{-| Server state
-}
module EasyBI.Server.State
  ( ServerState (..)
  , emptyState
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
    { ssViews   :: [(NiceHash (View Hashed), View Hashed)]
    , ssQueries :: Map (NiceHash TypedQueryExpr) TypedQueryExpr
    }

{-| Empty server state
-}
emptyState :: ServerState
emptyState = ServerState [] mempty

views :: Catalog -> [View Plain]
views =
  let mkView (names, typedQueryExpr) =
        View
          { vQuery = hPlain typedQueryExpr
          , vVisualisation = error "FIXME: visualisations"
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
      { ssViews = fmap (\(a, b, _) -> (a, b)) viewsH
      , ssQueries = Map.fromList $ fmap (\(_, a, c) -> (hNiceHash $ vQuery a, c)) viewsH
      }
