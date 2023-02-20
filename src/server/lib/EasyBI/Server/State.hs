{-# LANGUAGE NamedFieldPuns #-}
{-| Server state
-}
module EasyBI.Server.State
  ( ServerState (..)
  , cubes
  , emptyState
  , findCube
  , findQuery
  , stateFromList
  ) where

import Data.List                     (intercalate)
import Data.Map.Strict               (Map)
import Data.Map.Strict               qualified as Map
import EasyBI.Server.Cube            (Cube (..), hashCube, queryExpr)
import EasyBI.Server.Visualisation   (fields)
import EasyBI.Sql.Catalog            (Catalog (..), TypedQueryExpr (..))
import EasyBI.Sql.Effects.Types      (RowType (..), Tp (..), TyScheme (..))
import EasyBI.Util.NiceHash          (Hashed, NiceHash, Plain, hNiceHash,
                                      hPlain, niceHash)
import Language.SQL.SimpleSQL.Syntax (Name (..))

data ServerState =
  ServerState
    { ssCubes   :: Map (NiceHash (Cube Hashed)) (Cube Hashed)
    , ssQueries :: Map (NiceHash TypedQueryExpr) TypedQueryExpr
    }

{-| Empty server state
-}
emptyState :: ServerState
emptyState = ServerState mempty mempty

cubes :: Catalog -> [Cube Plain]
cubes =
  let mkCube (names, typedQueryExpr) =
        let cFields = case teType typedQueryExpr of
              TyScheme _ (TpRow (RowType _ mp)) -> fields mp
              _                                 -> []
        in Cube
            { cQuery = hPlain typedQueryExpr
            , cTitle = mkTitle names
            , cFields
            }
      mkTitle [] = "<no title>"
      mkTitle xs =
        let get (Name _ n) = n
        in intercalate "." (get <$> xs)
  in fmap mkCube . Map.toList . _views

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
