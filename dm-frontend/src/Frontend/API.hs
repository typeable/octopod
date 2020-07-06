{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

module Frontend.API where

import Control.Lens
import Data.Aeson
import Data.Proxy
import Data.Text as T
import Reflex.Dom as R
import Servant.API as S
import Servant.Reflex as SR

import Common.API
import Common.Types


apiClients
  :: forall t m. MonadWidget t m
  => SR.Client t m API ()
apiClients = clientWithOpts
  (Proxy @API)
  (Proxy @m)
  (Proxy @())
  (constDyn host)
  tweakRequest
  where
    tweakRequest = ClientOptions $ \req ->
      pure $ req
        & xhrRequest_config
        . xhrRequestConfig_headers
        . at "Authorization" ?~ auth
#ifdef DEVELOPMENT
    host = SR.BaseFullUrl SR.Https "dm-genfly-app.stage.thebestagent.pro" 443 "/"
    auth = "Basic dWk6cVoxZGRKMHZpWkRMbXlKaG5JNVh1QnZMQnZ3MkMwUVBqOTRaZ0VwOQ=="
#else
    host = SR.BaseFullUrl SR.Https "dm-genfly-app.stage.thebestagent.pro" 443 "/"
    auth = "Basic dWk6cVoxZGRKMHZpWkRMbXlKaG5JNVh1QnZMQnZ3MkMwUVBqOTRaZ0VwOQ=="
#endif


listEndpoint
  :: MonadWidget t m
  => Event t ()
  -> m (Event t (ReqResult () [DeploymentFullInfo]))
createEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text Deployment)
  -> Event t ()
  -> m (Event t (ReqResult () CommandResponse))
getEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text DeploymentName)
  -> Event t ()
  -> m (Event t (ReqResult () Deployment))
editEndpoint
  :: MonadWidget t m
  => Dynamic t (Either Text DeploymentName)
  -> Dynamic t (Either Text [EnvPair])
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
  :<|> getEndpoint
  :<|> editEndpoint
  :<|> deleteEndpoint
  :<|> updateEndpoint
  :<|> infoEndpoint
  :<|> statusEndpoint
  :<|> cleanupEndpoint
  :<|> restoreEndpoint)
  :<|> getActionInfoEndpoint
  :<|> pingEndpoint
  :<|> cleanArchiveEndpoint
  :<|> projectName = apiClients

commandResponse :: ReqResult tag CommandResponse -> Maybe CommandResponse
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
