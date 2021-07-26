{-# OPTIONS_GHC -Wno-orphans #-}

module Octopod.PowerAPI.Auth.Server
  ( AuthHeader (..),
  )
where

import Data.ByteString (ByteString)
import Network.Wai
import Octopod.PowerAPI
import Servant.Auth.Server
import Servant.Auth.Server.Internal.Class

newtype AuthHeader = AuthHeader ByteString

instance IsAuth AuthHeaderAuth () where
  type AuthArgs AuthHeaderAuth = '[AuthHeader]
  runAuth _ _ (AuthHeader h) = AuthCheck $ \req ->
    pure $ case lookup "Authorization" $ requestHeaders req of
      Just v | v == h -> Authenticated ()
      _ -> Indefinite

deriving anyclass instance ToJWT ()
