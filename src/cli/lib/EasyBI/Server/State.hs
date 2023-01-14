{-| Server state
-}
module EasyBI.Server.State
  ( ServerState (..)
  , emptyState
  , stateFromList
  ) where

import Data.Map.Strict      (Map)
import Data.Map.Strict      qualified as Map
import EasyBI.Server.View   (View (vQuery), WrappedQueryExpr, hashView,
                             queryExpr)
import EasyBI.Util.NiceHash (Hashed, NiceHash, Plain, hNiceHash, niceHash)

data ServerState =
  ServerState
    { ssViews   :: [(NiceHash (View Hashed), View Hashed)]
    , ssQueries :: Map (NiceHash WrappedQueryExpr) WrappedQueryExpr
    }

{-| Empty server state
-}
emptyState :: ServerState
emptyState = ServerState [] mempty

stateFromList :: [View Plain] -> ServerState
stateFromList views =
  let mkV v =
        let v' = hashView v
        in (niceHash v', v', queryExpr v)
      viewsH = mkV <$> views

  in ServerState
      { ssViews = fmap (\(a, b, _) -> (a, b)) viewsH
      , ssQueries = Map.fromList $ fmap (\(_, a, c) -> (hNiceHash $ vQuery a, c)) viewsH
      }
