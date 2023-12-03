{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}
module EasyBI.Server
  ( ServerConfig (..)
  , easyBIServer
  , runServer
  ) where

import Control.Monad               (when)
import Control.Monad.Except        (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class      (MonadIO (..))
import Data.Map                    (Map)
import Data.Map                    qualified as Map
import Data.Proxy                  (Proxy (..))
import Data.String                 (IsString (..))
import EasyBI.Server.API           (API)
import EasyBI.Server.Cube          (Cube, hashCube)
import EasyBI.Server.Eval          (APIQuery, DbConnectionPool, buildQuery,
                                    evalQuery)
import EasyBI.Server.State         (ServerState (..))
import EasyBI.Server.State         qualified as State
import EasyBI.Server.Visualisation (SqlFieldName, Visualisation)
import EasyBI.Server.Visualisation qualified as V
import EasyBI.Sql.Catalog          (TypedQueryExpr (..))
import EasyBI.Util.JSON            (WrappedObject (..))
import EasyBI.Util.NiceHash        (Hashed, NiceHash, Plain, WithHash, withHash)
import EasyBI.Vis.Types            (Selections)
import Network.Wai.Handler.Warp    qualified as Warp
import Servant.API                 ((:<|>) (..))
import Servant.Server              (Server, ServerError (..), err404, serve)

import Debug.Trace                 qualified as Debug

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
    :<|> pure ((fmap (fmap (hashCube . fst))) (cubes state))
    :<|> fmap (hashCube . fst) . cube state
    :<|> vis state
    :<|> eval pool state
  where
    health = pure ()
    cubes ServerState{ssCubes} = Map.toList ssCubes

vis :: (MonadError ServerError m) => ServerState -> NiceHash (Cube Hashed) -> Selections [] SqlFieldName -> m [WithHash (Visualisation (NiceHash (Cube Hashed)))]
vis state hsh selections = do
  (_, fields) <- cube state hsh
  sel' <- traverse (lookupFromMaybe (flip Map.lookup fields)) selections
  let result = V.visualisations hsh sel'
  when (null result) $ Debug.traceM $ "vis: no results!"
  pure (withHash <$> result)

cube :: (MonadError ServerError m) => ServerState -> NiceHash (Cube Hashed) -> m (Cube Plain, Map SqlFieldName V.Field)
cube state = lookupFromMaybe (State.findCube state)

lkp :: (MonadError ServerError m) => ServerState -> NiceHash (Cube Hashed) -> m TypedQueryExpr
lkp state = lookupFromMaybe (State.findQuery state)

eval :: (MonadFail m, MonadIO m, MonadError ServerError m) => DbConnectionPool -> ServerState -> NiceHash (Cube Hashed) -> APIQuery -> m [WrappedObject]
eval pool state hsh apiQuery = lkp state hsh >>= failOnError . buildQuery apiQuery . teQuery >>= liftIO . evalQuery pool

lookupFromMaybe :: (MonadError ServerError m, Show k) => (k -> Maybe v) -> k -> m v
lookupFromMaybe f k = case f k of
  Nothing ->
    let msg = "Not found: " <> show k
    in throwError $ err404 { errBody = fromString msg }
  Just a -> pure a

failOnError :: (MonadIO m, Show e) => ExceptT e m a -> m a
failOnError action = runExceptT action >>= \case
  Left err -> liftIO $ do
    putStrLn (show err)
    fail (show err)
  Right x -> pure x
