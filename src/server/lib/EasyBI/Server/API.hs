{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module EasyBI.Server.API
  ( API
  ) where

import           EasyBI.Server.View          (View)
import           EasyBI.Server.Visualisation (Visualisation)
import           EasyBI.Sql.Catalog          (TypedQueryExpr)
import           EasyBI.Util.JSON            (WrappedObject)
import           EasyBI.Util.NiceHash        (Hashed, NiceHash, WithHash)
import           Servant.API                 (Capture, Get, JSON, type (:<|>),
                                              type (:>))

type API =
  "api" :> (
    "health" :> Get '[JSON] ()
    :<|> "views" :> Get '[JSON] [WithHash (View Hashed)]
    :<|> "vis"   :> Capture "query" (NiceHash TypedQueryExpr) :> Get '[JSON] [Visualisation]
    :<|> "eval"  :> Capture "query" (NiceHash TypedQueryExpr) :> Get '[JSON] [WrappedObject]
  )
