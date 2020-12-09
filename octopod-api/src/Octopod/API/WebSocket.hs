{-# LANGUAGE CPP #-}

module Octopod.API.WebSocket where

#ifndef ghcjs_HOST_OS

import           Data.Aeson
import           Servant.API
import           Servant.API.WebSocketConduit

#endif


type ApiWSPath = "event"


#ifndef ghcjs_HOST_OS

-- | WS API
type WebSocketAPI = ApiWSPath :> WebSocketSource Value

#endif
