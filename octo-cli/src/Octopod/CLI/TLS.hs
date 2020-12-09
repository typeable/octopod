module Octopod.CLI.TLS
  ( makeClientManager
  ) where

import           Common.Types
import qualified Data.ByteString.Char8 as BSC
import           Data.Coerce
import           Data.Default.Class (def)
import           Network.Connection
import           Network.HTTP.Client
  (Manager, managerResponseTimeout, newManager, responseTimeoutMicro)
import           Network.HTTP.Client.TLS
import           Network.TLS
  (ClientHooks(..), clientHooks, clientSupported, credentialLoadX509,
  defaultParamsClient, supportedCiphers)
import           Network.TLS.Extra.Cipher

-- | Initializes the HTTP client using TLS
makeClientManager :: String -> TLSCertPath -> TLSKeyPath -> IO Manager
makeClientManager octopodHostName cert key = do
  creds <- do
    let
      cert' = BSC.unpack . coerce $ cert
      key'  = BSC.unpack . coerce $ key
    either error Just <$> credentialLoadX509 cert' key'
  let
    hooks               = def
      { onCertificateRequest = \_       -> return creds
      , onServerCertificate  = \_ _ _ _ -> return []
      }
    clientParams        = (defaultParamsClient octopodHostName  "")
      { clientHooks     = hooks
      , clientSupported = def { supportedCiphers = ciphersuite_default }
      }
    tlsSettings         = TLSSettings clientParams
    timeout             = 20 * 60 * 10 ^ (6 :: Int)
    tlsManagerSettings' = (mkManagerSettings tlsSettings Nothing)
      { managerResponseTimeout = responseTimeoutMicro timeout }
    in newManager tlsManagerSettings'
