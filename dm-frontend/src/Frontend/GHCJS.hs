{-|
Module      : Frontend.GHCJS
Description : GHCJS functions.

This module contains all functions that require GHCJS imports.
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
-- ^ API host key in session storage.
wsUrlKey = "ws"
-- ^ Websocket host key in session storage.
appAuthKey = "auth"
-- ^ API auth token key in session storage.

-- | There are project settings that contain the host for API requests,
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

-- | Reads settings from the config file and puts them into session storage.
-- Throws 'BadConfig' if the config file can't be parsed.
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

-- | Reads settings from the config file and puts them into session storage. If
-- the config file can't be parsed returns 'False', else 'True'.
initConfig :: MonadJSM m => m Bool
initConfig = liftJSM $ catchAll (initConfig' >> pure True) (const $ pure False)

-- | Gets a value from session storage for the given key.
-- Throws an error if key doesn't exist.
getVar
  :: MonadJSM m
  => JSString -- ^ Key in session storage.
  -> m T.Text -- ^ Value from session storage.
getVar k = do
  w <- currentWindowUnchecked
  stor <- getSessionStorage w
  x <- getItemUnsafe stor k
  pure $ pack x

-- | Gets the API host from session storage.
getAppUrl :: MonadJSM m => m T.Text
getAppUrl = getVar appUrlKey

-- | Gets the API auth token from session storage.
getAppAuth :: MonadJSM m => m T.Text
getAppAuth = getVar appAuthKey

-- | Gets the WS host from session storage.
getWsUrl :: MonadJSM m => m T.Text
getWsUrl = getVar wsUrlKey
