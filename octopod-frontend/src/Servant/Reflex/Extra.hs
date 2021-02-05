{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Reflex.Extra
  ( reqErrorBody
  ) where

import           Data.Functor.Identity
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text.Encoding as E
import           Reflex.Dom
import           Servant.API
import           Servant.Reflex

instance (SupportsServantReflex t m)
  => HasClient t m (NoContentVerb 'GET) tag where
  type Client t m (NoContentVerb 'GET) tag = Event t tag -> m (Event t (ReqResult tag NoContent))
  clientWithRouteAndResultHandler Proxy _ _ req baseurl opts wrap' trigs =
    wrap' =<< fmap  runIdentity <$> performRequestsNoBody method (constDyn $ Identity req) baseurl opts trigs
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy 'GET)

reqErrorBody :: ReqResult tag a -> Maybe Text
reqErrorBody (ResponseFailure _ _ XhrResponse {_xhrResponse_responseText = Just b})
  = Just b
reqErrorBody (ResponseFailure _ _ XhrResponse {_xhrResponse_response = Just (XhrResponseBody_Default t)}) =
  Just t
reqErrorBody (ResponseFailure _ _ XhrResponse {_xhrResponse_response = Just (XhrResponseBody_Text t)}) =
  Just t
reqErrorBody (ResponseFailure _ _ XhrResponse {_xhrResponse_response = Just (XhrResponseBody_ArrayBuffer t)}) =
  Just . E.decodeUtf8 $ t
reqErrorBody x = reqFailure x
