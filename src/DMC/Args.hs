module DMC.Args where

import Data.ByteString (ByteString)
import Data.Coerce
import Options.Generic

import Types

-- | CLI options
-- FIXME: add options documentation
data Args
  = Create
    { name        :: Text
    , tag         :: Text
    , envs        :: [Text]
    , tlsCertPath :: Maybe ByteString
    , tlsKeyPath  :: Maybe ByteString }
  | List
    { tlsCertPath :: Maybe ByteString
    , tlsKeyPath  :: Maybe ByteString }
  | Edit
    { name        :: Text
    , tlsCertPath :: Maybe ByteString
    , tlsKeyPath  :: Maybe ByteString }
  | Delete
    { name        :: Text
    , tlsCertPath :: Maybe ByteString
    , tlsKeyPath  :: Maybe ByteString }
  | Update
    { name        :: Text
    , tag         :: Text
    , tlsCertPath :: Maybe ByteString
    , tlsKeyPath  :: Maybe ByteString }
  | Info
    { name        :: Text
    , tlsCertPath :: Maybe ByteString
    , tlsKeyPath  :: Maybe ByteString }
  | Cleanup
    { name        :: Text
    , tlsCertPath :: Maybe ByteString
    , tlsKeyPath  :: Maybe ByteString }
  deriving stock (Show, Generic)

instance ParseRecord Args where
  -- FIXME: external CLI interface depends on the naming in the code, yuck
  parseRecord = parseRecordWithModifiers $ defaultModifiers
    { shortNameModifier = firstLetter }

data DMCArgs
  = CreateC
    { deploymentName :: Text
    , deploymentTag  :: Text
    , deploymentEnvs :: [Text]
    , dmcTLSCertPath :: Maybe TLSCertPath
    , dmcTLSKeyPath  :: Maybe TLSKeyPath }
  | ListC
    { dmcTLSCertPath :: Maybe TLSCertPath
    , dmcTLSKeyPath  :: Maybe TLSKeyPath }
  | EditC
    { deploymentName :: Text
    , dmcTLSCertPath :: Maybe TLSCertPath
    , dmcTLSKeyPath  :: Maybe TLSKeyPath }
  | DeleteC
    { deploymentName :: Text
    , dmcTLSCertPath :: Maybe TLSCertPath
    , dmcTLSKeyPath  :: Maybe TLSKeyPath }
  | UpdateC
    { deploymentName :: Text
    , deploymentTag  :: Text
    , dmcTLSCertPath :: Maybe TLSCertPath
    , dmcTLSKeyPath  :: Maybe TLSKeyPath }
  | InfoC
    { deploymentName :: Text
    , dmcTLSCertPath :: Maybe TLSCertPath
    , dmcTLSKeyPath  :: Maybe TLSKeyPath }
  | CleanupC
    { deploymentName :: Text
    , dmcTLSCertPath :: Maybe TLSCertPath
    , dmcTLSKeyPath  :: Maybe TLSKeyPath }
  deriving stock (Show, Generic)

parseArgs :: IO DMCArgs
parseArgs = do
  args <- getRecord "DMC"
  pure $ case args of
    Create  n t e c k -> CreateC n t e (coerce <$> c) (coerce <$> k)
    List    c k       -> ListC (coerce <$> c) (coerce <$> k)
    Edit    n c k     -> EditC n (coerce <$> c) (coerce <$> k)
    Delete n c k      -> DeleteC n (coerce <$> c) (coerce <$> k)
    Update  n t c k   -> UpdateC n t (coerce <$> c) (coerce <$> k)
    Info    n c k     -> InfoC n (coerce <$> c) (coerce <$> k)
    Cleanup n c k     -> CleanupC n (coerce <$> c) (coerce <$> k)
