{-# LANGUAGE NamedFieldPuns #-}
{-| Server state
-}
module EasyBI.Server.State
  ( ServerState (..)
  , emptyState
  , findCube
  , findQuery
  , stateFromList
  ) where

import Data.Map.Strict      (Map)
import Data.Map.Strict      qualified as Map
import EasyBI.Server.Cube   (Cube (..), hashCube, queryExpr)
import EasyBI.Sql.Catalog   (TypedQueryExpr (..))
import EasyBI.Util.NiceHash (Hashed, NiceHash, Plain, hNiceHash, niceHash)

data ServerState =
  ServerState
    { ssCubes   :: Map (NiceHash (Cube Hashed)) (Cube Hashed)
    , ssQueries :: Map (NiceHash TypedQueryExpr) TypedQueryExpr
    }

{-| Empty server state
-}
emptyState :: ServerState
emptyState = ServerState mempty mempty

stateFromList :: [Cube Plain] -> ServerState
stateFromList cubes_ =
  let mkV v =
        let v' = hashCube v
        in (niceHash v', v', queryExpr v)
      cubesH = mkV <$> cubes_

  in ServerState
      { ssCubes = Map.fromList (fmap (\(a, b, _) -> (a, b)) cubesH)
      , ssQueries = Map.fromList $ fmap (\(_, a, c) -> (hNiceHash $ cQuery a, c)) cubesH
      }

-- | Lookup the query definition
findQuery :: ServerState -> NiceHash TypedQueryExpr -> Maybe TypedQueryExpr
findQuery ServerState{ssQueries} h = Map.lookup h ssQueries

findCube :: ServerState -> NiceHash (Cube Hashed) -> Maybe (Cube Hashed)
findCube ServerState{ssCubes} h = Map.lookup h ssCubes
