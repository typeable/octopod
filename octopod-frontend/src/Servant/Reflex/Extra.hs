{-# OPTIONS_GHC -Wno-orphans #-}

module Servant.Reflex.Extra () where

import qualified Data.Text.Encoding as E
import Servant.Reflex
import Servant.API
import Reflex.Dom
import Data.Proxy
import Data.Functor.Identity

instance (SupportsServantReflex t m)
  => HasClient t m (NoContentVerb 'GET) tag where
  type Client t m (NoContentVerb 'GET) tag = Event t tag -> m (Event t (ReqResult tag NoContent))
  clientWithRouteAndResultHandler Proxy _ _ req baseurl opts wrap' trigs =
    wrap' =<< fmap  runIdentity <$> performRequestsNoBody method (constDyn $ Identity req) baseurl opts trigs
      where method = E.decodeUtf8 $ reflectMethod (Proxy :: Proxy 'GET)
