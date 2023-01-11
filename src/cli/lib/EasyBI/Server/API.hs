{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module EasyBI.Server.API
  ( API
  , Hashed
  ) where

import EasyBI.Server.View   (View)
import EasyBI.Util.NiceHash (Hashed, NiceHash)
import Servant.API          (Get, JSON, type (:<|>), type (:>))

type WithHash a = (NiceHash a, a)

type API =
  "health" :> Get '[JSON] ()
  :<|> "views" :> Get '[JSON] [WithHash (View Hashed)]
