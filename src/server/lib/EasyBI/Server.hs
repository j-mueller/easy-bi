{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
module EasyBI.Server
  ( ServerConfig (..)
  , easyBIServer
  , runServer
  ) where

import Data.Proxy               (Proxy (..))
import EasyBI.Server.API        (API)
import EasyBI.Server.State      (ServerState (..))
import Network.Wai.Handler.Warp qualified as Warp
import Servant.API              ((:<|>) (..))
import Servant.Server           (Server, serve)

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
easyBIServer state = health :<|> views state where
  health = pure ()
  views ServerState{ssViews} = pure ssViews
