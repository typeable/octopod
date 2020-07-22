{-|
Module      : Frontend.GHCJS
Description : API requests.

This module contains client functions that perform API requests (these functions
are generated from servant API) and functions that work with requests results.
-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

module Frontend.API where

import Control.Lens
import Data.Aeson
import Data.Proxy
import Data.Text as T
import GHC.TypeLits
import Reflex.Dom as R
import Servant.API as S
import Servant.Reflex as SR

import Common.API
import Common.Types
import Frontend.GHCJS

-- | 'apiClients' generates API requests. Before each request API host and API
-- authentication token are read from session storage.
apiClients
  :: forall t m. MonadWidget t m
  => SR.Client t m API ()
apiClients =
  clientWithOpts
    (Proxy @API)
    (Proxy @m)
    (Proxy @())
    (constDyn host)
    tweakRequest
  where
    tweakRequest = ClientOptions $ \req -> do
      url <- getAppUrl
      auth <- getAppAuth
      pure $ req
        & xhrRequest_config
        . xhrRequestConfig_headers
        . at "Authorization" ?~ auth
        & xhrRequest_url %~ (T.append url)
    host = SR.BasePath "/"


listEndpoint
  :: MonadWidget t m
  => Event t ()
  -> m (Event t (ReqResult () [DeploymentFullInfo]))
createEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text Deployment)
  -> Event t ()
  -> m (Event t (ReqResult () CommandResponse))
deleteEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text DeploymentName)
  -> Event t ()
  -> m (Event t (ReqResult () CommandResponse))
updateEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text DeploymentName)
  -> Dynamic t (Either Text DeploymentUpdate)
  -> Event t ()
  -> m (Event t (ReqResult () CommandResponse))
infoEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text DeploymentName)
  -> Event t ()
  -> m (Event t (ReqResult () [DeploymentInfo]))
fullInfoEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text DeploymentName)
  -> Event t ()
  -> m (Event t (ReqResult () DeploymentFullInfo))
statusEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text DeploymentName)
  -> Event t ()
  -> m (Event t (ReqResult () CurrentDeploymentStatus))
cleanupEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text DeploymentName)
  -> Event t ()
  -> m (Event t (ReqResult () CommandResponse))
restoreEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text DeploymentName)
  -> Event t ()
  -> m (Event t (ReqResult () CommandResponse))
getActionInfoEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text ActionId)
  -> Event t ()
  -> m (Event t (ReqResult () ActionInfo))
pingEndpoint
  :: MonadWidget t m
  => Event t ()
  -> m (Event t (ReqResult () NoContent))
cleanArchiveEndpoint
  :: MonadWidget t m
  => Event t ()
  -> m (Event t (ReqResult () CommandResponse))
projectName
  :: MonadWidget t m
  => Event t ()
  -> m (Event t (ReqResult () ProjectName))
(listEndpoint
  :<|> createEndpoint
  :<|> deleteEndpoint
  :<|> updateEndpoint
  :<|> infoEndpoint
  :<|> fullInfoEndpoint
  :<|> statusEndpoint
  :<|> cleanupEndpoint
  :<|> restoreEndpoint)
  :<|> getActionInfoEndpoint
  :<|> pingEndpoint
  :<|> cleanArchiveEndpoint
  :<|> projectName = apiClients

-- | Parser for kubectl command response.
commandResponse
  :: ReqResult tag CommandResponse -- ^ Request result with command response.
  -> Maybe CommandResponse
commandResponse = \case
  ResponseSuccess _ a _   -> Just a
  ResponseFailure _ _ xhr ->
    xhr ^? xhrResponse_response . _Just
      . to getArrayBufferBody . _Just
      . to decodeStrict . _Just
  RequestFailure _ _      -> Nothing
  where
    getArrayBufferBody = \case
      XhrResponseBody_ArrayBuffer x -> Just x
      _                             -> Nothing

-- | Gets path of websocket url.
wsPath :: Text
wsPath = T.pack $ symbolVal (Proxy @ApiWSPath)

-- | Helper for event with request result.
processResp
  :: Reflex t
  => Event t (ReqResult tag a)
  -- ^ Event with request result.
  -> (Event t a, Event t ())
  -- ^ (Success event with obtained data, failure event).
processResp respEv =
  let
    respOkEv = fmapMaybe reqSuccess respEv
    errEv = fmapMaybe reqFailure respEv
  in (respOkEv, () <$ errEv)
