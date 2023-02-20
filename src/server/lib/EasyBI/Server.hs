{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
module EasyBI.Server
  ( ServerConfig (..)
  , easyBIServer
  , runServer
  ) where

import Control.Monad.Except        (MonadError (..))
import Control.Monad.IO.Class      (MonadIO (..))
import Data.Map                    qualified as Map
import Data.Proxy                  (Proxy (..))
import Data.String                 (IsString (..))
import EasyBI.Server.API           (API)
import EasyBI.Server.Cube          (Cube)
import EasyBI.Server.Eval          (DbConnectionPool, evalQuery, restrictTo)
import EasyBI.Server.State         (ServerState (..))
import EasyBI.Server.State         qualified as State
import EasyBI.Server.Visualisation (Field, Visualisation)
import EasyBI.Server.Visualisation qualified as V
import EasyBI.Sql.Catalog          (TypedQueryExpr (..))
import EasyBI.Util.JSON            (WrappedObject (..))
import EasyBI.Util.NiceHash        (Hashed, NiceHash)
import EasyBI.Vis.Types            (Selections)
import Network.Wai.Handler.Warp    qualified as Warp
import Servant.API                 ((:<|>) (..))
import Servant.Server              (Server, ServerError (..), err404, serve)

runServer :: DbConnectionPool -> ServerState -> ServerConfig -> IO ()
runServer pool state ServerConfig{scPort} =
  let app = serve (Proxy @API) (easyBIServer pool state)
  in Warp.run scPort app

data ServerConfig =
  ServerConfig
    { scPort :: Int -- ^ Port for the server app
    }
    deriving (Eq, Ord, Show)

easyBIServer :: DbConnectionPool -> ServerState -> Server API
easyBIServer pool state =
  health
    :<|> cubes state
    :<|> cube state
    :<|> vis
    :<|> eval pool state
  where
    health = pure ()
    cubes ServerState{ssCubes} = pure (Map.toList ssCubes)

vis :: (MonadError ServerError m) => NiceHash TypedQueryExpr -> Selections Field -> m [Visualisation (NiceHash TypedQueryExpr)]
vis hsh selections = pure (V.visualisations hsh selections)

cube :: (MonadError ServerError m) => ServerState -> NiceHash (Cube Hashed) -> m (Cube Hashed)
cube state = lookupFromMaybe (State.findCube state)

lkp :: (MonadError ServerError m) => ServerState -> NiceHash TypedQueryExpr -> m TypedQueryExpr
lkp state = lookupFromMaybe (State.findQuery state)

eval :: (MonadFail m, MonadIO m, MonadError ServerError m) => DbConnectionPool -> ServerState -> NiceHash TypedQueryExpr -> [String] -> m [WrappedObject]
eval pool state hsh fieldNames = lkp state hsh >>= restrictTo fieldNames . teQuery >>= liftIO . evalQuery pool

lookupFromMaybe :: (MonadError ServerError m, Show k) => (k -> Maybe v) -> k -> m v
lookupFromMaybe f k = case f k of
  Nothing ->
    let msg = "Not found: " <> show k
    in throwError $ err404 { errBody = fromString msg }
  Just a -> pure a
