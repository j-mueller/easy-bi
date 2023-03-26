{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module EasyBI.Server.API
  ( API
  ) where

import EasyBI.Server.Cube          (Cube)
import EasyBI.Server.Visualisation (Field, Visualisation)
import EasyBI.Sql.Catalog          (TypedQueryExpr)
import EasyBI.Util.JSON            (WrappedObject)
import EasyBI.Util.NiceHash        (Hashed, NiceHash, WithHash)
import EasyBI.Vis.Types            (Selections)
import Servant.API                 (Capture, Get, JSON, Post, ReqBody,
                                    type (:<|>), type (:>))

type API =
  "api" :> (
    "health" :> Get '[JSON] ()
    :<|> "cubes" :> Get '[JSON] [WithHash (Cube Hashed)]
    :<|> "cubes" :> Capture "cube"  (NiceHash (Cube Hashed))  :> Get '[JSON] (Cube Hashed)
    :<|> "vis"   :> Capture "query" (NiceHash TypedQueryExpr) :> ReqBody '[JSON] (Selections [] Field) :> Post '[JSON] [Visualisation (NiceHash TypedQueryExpr)]
    :<|> "eval"  :> Capture "query" (NiceHash TypedQueryExpr) :> ReqBody '[JSON] [Field] :> Post '[JSON] [WrappedObject]
  )
