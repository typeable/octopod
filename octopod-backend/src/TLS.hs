{-|
Module      : TLS
Description : TLS utils.

This module contains TLS utils.
-}


module TLS where


import qualified Data.ByteString.Char8 as BSC
import           Data.Default.Class (def)
import           Data.X509 (ExtKeyUsagePurpose(..), HashALG(HashSHA256))
import           Data.X509.CertificateStore (readCertificateStore)
import           Data.X509.Validation
  (ValidationChecks(..), ValidationHooks(..), checkLeafV3, validate)
import           Network.TLS
  (CertificateRejectReason(..), CertificateUsage(..), onClientCertificate)
import           Network.Wai.Handler.WarpTLS hiding (TLSSettings, tlsSettings)
import qualified Network.Wai.Handler.WarpTLS as WTLS


import           Types


-- | Creates TLS options for the HTTP server
createTLSOpts
  :: TLSCertPath
  -> TLSKeyPath
  -> TLSStorePath
  -> ServerPort
  -> WTLS.TLSSettings
createTLSOpts tlsCert tlsKey tlsStore serverPort =
  initTLSSettings
    { tlsWantClientCert = True
    , onInsecure        = DenyInsecure
      "This server only accepts secure HTTPS connections."
    , tlsServerHooks    = def { onClientCertificate =
      fmap certificateUsageFromValidations . validateCertificate }
    }
  where
    tlsCert'                        = unTLSCertPath tlsCert
    tlsKey'                         = unTLSKeyPath tlsKey
    tlsStore'                       = BSC.unpack . unTLSStorePath $ tlsStore
    serverPort'                     = unServerPort serverPort
    initTLSSettings                 = WTLS.tlsSettings
      (BSC.unpack tlsCert') (BSC.unpack tlsKey')
    certificateUsageFromValidations = maybe CertificateUsageAccept
      (CertificateUsageReject . CertificateRejectOther)
    serviceID                       = ("localhost", BSC.pack $ show serverPort')
    validationHooks                 = def
      { hookValidateName = \_ _ -> [] }
    validationChecks = def
      { checkStrictOrdering = True
      , checkLeafKeyPurpose = [KeyUsagePurpose_ClientAuth]
      , checkLeafV3         = False
      }
    validateCertificate cert = do
      mstore <- readCertificateStore tlsStore'
      maybe
        (pure . Just $ "Cannot init a store")
        (fmap fromX509FailedReasons . (\store -> validate HashSHA256
          validationHooks validationChecks store def serviceID cert))
        mstore
    fromX509FailedReasons reasons = case reasons of
      [] -> Nothing
      _ -> Just (show reasons)
