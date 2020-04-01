module TLS where


import qualified Data.ByteString.Char8 as BSC
import Data.Default.Class (def)
import Data.X509 (ExtKeyUsagePurpose(..), HashALG(HashSHA256))
import Data.X509.CertificateStore (readCertificateStore)
import Data.X509.Validation
  (ValidationChecks(..), ValidationHooks(..), checkLeafV3, validate)
import Network.Connection
import Network.HTTP.Client
  ( Manager, managerResponseTimeout, newManager
  , responseTimeoutMicro)
import Network.HTTP.Client.TLS
import Network.TLS
  ( CertificateRejectReason(..), CertificateUsage(..), ClientHooks(..)
  , clientHooks, clientSupported, credentialLoadX509, defaultParamsClient
  , onClientCertificate, supportedCiphers)
import Network.TLS.Extra.Cipher
import qualified Network.Wai.Handler.WarpTLS as WTLS
import Network.Wai.Handler.WarpTLS hiding (TLSSettings, tlsSettings)


import Types


createTLSOpts :: TLSCertPath -> TLSKeyPath -> TLSStorePath -> ServerPort -> WTLS.TLSSettings
createTLSOpts tlsCert tlsKey tlsStore serverPort
  = initTLSSettings
      { tlsWantClientCert = True
      , onInsecure = DenyInsecure "This server only accepts secure HTTPS connections."
      , tlsServerHooks = def { onClientCertificate =
          fmap certificateUsageFromValidations . validateCertificate }
      }
  where
    tlsCert' = unTLSCertPath tlsCert
    tlsKey' = unTLSKeyPath tlsKey
    tlsStore' = BSC.unpack . unTLSStorePath $ tlsStore
    serverPort' = unServerPort serverPort
    initTLSSettings = WTLS.tlsSettings (BSC.unpack tlsCert') (BSC.unpack tlsKey')
    certificateUsageFromValidations = maybe CertificateUsageAccept
      (CertificateUsageReject . CertificateRejectOther)
    serviceID = ("localhost", BSC.pack $ show serverPort')
    validationHooks = def
      { hookValidateName = \_ _ -> [] }
    validationChecks = def
      { checkStrictOrdering = True
      , checkLeafKeyPurpose = [KeyUsagePurpose_ClientAuth]
      , checkLeafV3 = False
      }
    validateCertificate cert = do
      mstore <- readCertificateStore tlsStore'
      maybe
        (pure $ Just "Cannot init a store")
        (fmap fromX509FailedReasons . (\store -> validate HashSHA256
          validationHooks validationChecks store def serviceID cert))
        mstore
    fromX509FailedReasons reasons = case reasons of
      [] -> Nothing
      _  -> Just (show reasons)

makeClientManager :: String -> FilePath -> FilePath -> IO Manager
makeClientManager dmsHostName cert key = do
  creds <- either error Just `fmap` credentialLoadX509 cert key
  let hooks = def
               { onCertificateRequest = \_ -> return creds
               , onServerCertificate = \_ _ _ _ -> return []
               }
      clientParams = (defaultParamsClient dmsHostName  "")
                      { clientHooks = hooks
                      , clientSupported = def { supportedCiphers = ciphersuite_default }
                      }
      tlsSettings = TLSSettings clientParams
      tlsManagerSettings' = (mkManagerSettings tlsSettings Nothing)
                              { managerResponseTimeout =
                                responseTimeoutMicro $ 20 * 60 * 10 ^ (6 :: Int) }
      in newManager tlsManagerSettings'

