{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
module EasyBI.Server
  ( ServerConfig (..)
  , easyBIServer
  , runServer
  ) where

import Control.Monad.Except        (MonadError (..))
import Data.Proxy                  (Proxy (..))
import Data.String                 (IsString (..))
import EasyBI.Server.API           (API)
import EasyBI.Server.State         (ServerState (..))
import EasyBI.Server.State         qualified as State
import EasyBI.Server.Visualisation (Visualisation)
import EasyBI.Server.Visualisation qualified as V
import EasyBI.Sql.Catalog          (TypedQueryExpr (..))
import EasyBI.Util.NiceHash        (NiceHash)
import Network.Wai.Handler.Warp    qualified as Warp
import Servant.API                 ((:<|>) (..))
import Servant.Server              (Server, ServerError (..), err404, serve)

runServer :: ServerState -> ServerConfig -> IO ()
runServer state ServerConfig{scPort} =
  let app = serve (Proxy @API) (easyBIServer state)
  in Warp.run scPort app

data ServerConfig =
  ServerConfig
    { scPort :: Int -- ^ Port for the server app
    }
    deriving (Eq, Ord, Show)

easyBIServer :: ServerState -> Server API
easyBIServer state =
  health
    :<|> views state
    :<|> vis state
    :<|> eval
  where
    health = pure ()
    views ServerState{ssViews} = pure ssViews
    eval _x = pure []

vis :: (MonadError ServerError m) => ServerState -> NiceHash TypedQueryExpr -> m [Visualisation]
vis state hsh = case State.findQuery state hsh of
  Nothing ->
    let msg = "Not found: " <> show hsh
    in throwError $ err404 { errBody = fromString msg }
  Just TypedQueryExpr{teType} -> pure (V.visualisations teType)
