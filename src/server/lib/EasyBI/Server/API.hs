{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module EasyBI.Server.API
  ( API
  ) where

import EasyBI.Server.Cube          (Cube)
import EasyBI.Server.Eval          (APIQuery)
import EasyBI.Server.Visualisation (SqlFieldName, Visualisation)
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
    :<|> "vis"   :> Capture "query" (NiceHash (Cube Hashed)) :> ReqBody '[JSON] (Selections [] SqlFieldName) :> Post '[JSON] [WithHash (Visualisation (NiceHash (Cube Hashed)))]
    :<|> "eval"  :> Capture "query" (NiceHash (Cube Hashed)) :> ReqBody '[JSON] APIQuery :> Post '[JSON] [WrappedObject]
  )
