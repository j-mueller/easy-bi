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

import Data.Map.Strict             (Map)
import Data.Map.Strict             qualified as Map
import EasyBI.Server.Cube          (Cube (..), fields, hashCube, queryExpr)
import EasyBI.Server.Visualisation (Field, SqlFieldName)
import EasyBI.Sql.Catalog          (TypedQueryExpr (..))
import EasyBI.Util.NiceHash        (Hashed, NiceHash, Plain, hNiceHash,
                                    niceHash)

data ServerState =
  ServerState
    { ssCubes   :: Map (NiceHash (Cube Hashed)) (Cube Plain, Map SqlFieldName Field)
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
        in (niceHash v', v, queryExpr v)
      cubesH = mkV <$> cubes_

  in ServerState
      { ssCubes = Map.fromList (fmap (\(a, b, _) -> (a, (b, fields b))) cubesH)
      , ssQueries = Map.fromList $ fmap (\(_, a, c) -> (hNiceHash $ cQuery a, c)) cubesH
      }

-- | Lookup the query definition
findQuery :: ServerState -> NiceHash (Cube Hashed) -> Maybe TypedQueryExpr
findQuery ServerState{ssCubes} h = fmap (queryExpr . fst) (Map.lookup h ssCubes)

findCube :: ServerState -> NiceHash (Cube Hashed) -> Maybe (Cube Plain, Map SqlFieldName Field)
findCube ServerState{ssCubes} h = Map.lookup h ssCubes
