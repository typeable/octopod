{-|
Module      : Frontend.GHCJS
Description : GHCJS functions.

This module contains all the functions that require GHCJS imports.
-}

module Frontend.GHCJS where

import Data.Aeson as A (Value, fromJSON, Result(..))
import Control.Monad.Catch
import Data.Text as T (pack, Text)
import Deriving.Aeson.Stock
import GHCJS.DOM
import GHCJS.DOM.Response
import GHCJS.DOM.Storage
import GHCJS.DOM.Types
import GHCJS.DOM.Window


appUrlKey, wsUrlKey, appAuthKey :: JSString
appUrlKey = "app"
-- ^ API host keys for session storage.
wsUrlKey = "ws"
-- ^ Websocket host key for session storage.
appAuthKey = "auth"
-- ^ API auth token key for session storage.

-- | There is an internal project settings that contain a host for API requests,
-- websocket host and API authentication token.
data ProjectConfig = ProjectConfig
  { appUrl :: String  -- ^ API host.
  , wsUrl :: String   -- ^ WS host.
  , appAuth :: String -- ^ API authentication token.
  }
  deriving (Generic, Show, Eq)
  deriving (FromJSON, ToJSON) via Snake ProjectConfig

-- | The type for error handling.
data BadConfig = BadConfig
  deriving (Show)

instance Exception BadConfig

-- | Reads settings from config file and puts them into storage session. Throws
-- 'BadConfig' in case the config file can't be parsed.
initConfig' :: JSM ()
initConfig' = do
  w <- currentWindowUnchecked
  resp <- fetch w ("/config.json" :: JSString) Nothing
  jsonVal <- json resp
  val :: A.Value <- fromJSValUnchecked jsonVal
  let mcfg = fromJSON val
  case mcfg of
    A.Success cfg -> do
      stor <- getSessionStorage w
      setItem stor appUrlKey $ appUrl cfg
      setItem stor wsUrlKey $ wsUrl cfg
      setItem stor appAuthKey $ appAuth cfg
    A.Error _ -> throwM BadConfig

-- | Reads settings from config file and puts them into storage session. If
-- config file can't be parsed returns False, else True.
initConfig :: MonadJSM m => m Bool
initConfig = liftJSM $ catchAll (initConfig' >> pure True) (const $ pure False)

-- | Gets value from session storage by key.
getVar
  :: MonadJSM m
  => JSString -- ^ Key in session storage.
  -> m T.Text
getVar k = do
  w <- currentWindowUnchecked
  stor <- getSessionStorage w
  x <- getItemUnsafe stor k
  pure $ pack x

-- | Gets API host from session storage.
getAppUrl :: MonadJSM m => m T.Text
getAppUrl = getVar appUrlKey

-- | Gets API auth from session storage.
getAppAuth :: MonadJSM m => m T.Text
getAppAuth = getVar appAuthKey

-- | Gets WS host from session storage.
getWsUrl :: MonadJSM m => m T.Text
getWsUrl = getVar wsUrlKey
