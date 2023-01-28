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
import Data.Proxy                  (Proxy (..))
import Data.String                 (IsString (..))
import EasyBI.Server.API           (API)
import EasyBI.Server.Eval          (DbConnectionPool, evalQuery)
import EasyBI.Server.State         (ServerState (..))
import EasyBI.Server.State         qualified as State
import EasyBI.Server.Visualisation (Visualisation)
import EasyBI.Server.Visualisation qualified as V
import EasyBI.Sql.Catalog          (TypedQueryExpr (..))
import EasyBI.Util.JSON            (WrappedObject (..))
import EasyBI.Util.NiceHash        (NiceHash)
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
    :<|> views state
    :<|> vis state
    :<|> eval pool state
  where
    health = pure ()
    views ServerState{ssViews} = pure ssViews

vis :: (MonadError ServerError m) => ServerState -> NiceHash TypedQueryExpr -> m [Visualisation]
vis state hsh = V.visualisations . teType <$> lkp state hsh

lkp :: (MonadError ServerError m) => ServerState -> NiceHash TypedQueryExpr -> m TypedQueryExpr
lkp state hsh = case State.findQuery state hsh of
  Nothing ->
    let msg = "Not found: " <> show hsh
    in throwError $ err404 { errBody = fromString msg }
  Just a -> pure a

eval :: (MonadIO m, MonadError ServerError m) => DbConnectionPool -> ServerState -> NiceHash TypedQueryExpr -> m [WrappedObject]
eval pool state hsh = lkp state hsh >>= liftIO . evalQuery pool . teQuery
