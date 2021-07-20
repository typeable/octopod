{-# OPTIONS_GHC -Wno-orphans #-}

module Octopod.PowerAPI.Auth.Client
  ( IsAuth (..),
    AuthContext (..),
  )
where

import Data.Proxy
import Octopod.PowerAPI
import Servant.API hiding (addHeader)
import Servant.Auth
import Servant.Client.Core

class IsAuth a where
  data AuthContext a
  applyAuth :: AuthContext a -> Request -> Request

instance (IsAuth a, HasClient m api) => HasClient m (Auth '[a] x :> api) where
  type Client m (Auth '[a] x :> api) = AuthContext a -> Client m api
  clientWithRoute pm Proxy req val =
    clientWithRoute pm (Proxy :: Proxy api) (applyAuth val req)

  hoistClientMonad pm _ f cl = hoistClientMonad pm (Proxy :: Proxy api) f . cl

instance IsAuth AuthHeaderAuth where
  data AuthContext AuthHeaderAuth = AuthHeaderAuthCtx String
  applyAuth (AuthHeaderAuthCtx h) req = addHeader "Authorization" h req
