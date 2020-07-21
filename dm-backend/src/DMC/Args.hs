module DMC.Args where

import Data.ByteString (ByteString)
import Data.Coerce
import Data.Semigroup ((<>))
import Data.Text
import Options.Applicative


import Types


data Args
  = Create
    { name                  :: Text
    , tag                   :: Text
    , setAppOverrides       :: [Text]
    , setStagingOverrides   :: [Text]
    , tlsCertPath           :: Maybe ByteString
    , tlsKeyPath            :: Maybe ByteString }
  | List
    { tlsCertPath           :: Maybe ByteString
    , tlsKeyPath            :: Maybe ByteString }
  | Delete
    { name                  :: Text
    , tlsCertPath           :: Maybe ByteString
    , tlsKeyPath            :: Maybe ByteString }
  | Update
    { name                  :: Text
    , tag                   :: Text
    , setAppOverrides       :: [Text]
    , unsetAppOverrides     :: [Text]
    , setStagingOverrides   :: [Text]
    , unsetStagingOverrides :: [Text]
    , tlsCertPath           :: Maybe ByteString
    , tlsKeyPath            :: Maybe ByteString }
  | Info
    { name                  :: Text
    , tlsCertPath           :: Maybe ByteString
    , tlsKeyPath            :: Maybe ByteString }
  | Cleanup
    { name                  :: Text
    , tlsCertPath           :: Maybe ByteString
    , tlsKeyPath            :: Maybe ByteString }
  | Restore
    { name                  :: Text
    , tlsCertPath           :: Maybe ByteString
    , tlsKeyPath            :: Maybe ByteString }
  | CleanArchive
    { tlsCertPath           :: Maybe ByteString
    , tlsKeyPath            :: Maybe ByteString }
  deriving stock (Show)

data DMCArgs
  = CreateC
    { deploymentName                 :: Text
    , deploymentTag                  :: Text
    , deploymentSetAppOverrides      :: [Text]
    , deploymentSetStaingOverrides   :: [Text]
    , dmcTLSCertPath                 :: Maybe TLSCertPath
    , dmcTLSKeyPath                  :: Maybe TLSKeyPath }
  | ListC
    { dmcTLSCertPath                 :: Maybe TLSCertPath
    , dmcTLSKeyPath                  :: Maybe TLSKeyPath }
  | DeleteC
    { deploymentName                 :: Text
    , dmcTLSCertPath                 :: Maybe TLSCertPath
    , dmcTLSKeyPath                  :: Maybe TLSKeyPath }
  | UpdateC
    { deploymentName                 :: Text
    , deploymentTag                  :: Text
    , deploymentSetAppOverrides      :: [Text]
    , deploymentUnsetAppOverrides    :: [Text]
    , deploymentSetStaingOverrides   :: [Text]
    , deploymentUnsetStaingOverrides :: [Text]
    , dmcTLSCertPath                 :: Maybe TLSCertPath
    , dmcTLSKeyPath                  :: Maybe TLSKeyPath }
  | InfoC
    { deploymentName                 :: Text
    , dmcTLSCertPath                 :: Maybe TLSCertPath
    , dmcTLSKeyPath                  :: Maybe TLSKeyPath }
  | CleanupC
    { deploymentName                 :: Text
    , dmcTLSCertPath                 :: Maybe TLSCertPath
    , dmcTLSKeyPath                  :: Maybe TLSKeyPath }
  | RestoreC
    { deploymentName                 :: Text
    , dmcTLSCertPath                 :: Maybe TLSCertPath
    , dmcTLSKeyPath                  :: Maybe TLSKeyPath }
  | CleanArchiveC
    { dmcTLSCertPath                 :: Maybe TLSCertPath
    , dmcTLSKeyPath                  :: Maybe TLSKeyPath }
  deriving stock (Show)

parseArgs :: IO DMCArgs
parseArgs = do
  args <- execParser opts
  pure $ case args of
    Create       n t e o c k       ->
      CreateC n t e o (coerce <$> c) (coerce <$> k)
    List         c k               -> ListC (coerce <$> c) (coerce <$> k)
    Delete       n c k             -> DeleteC n (coerce <$> c) (coerce <$> k)
    Update       n t e ue o uo c k ->
      UpdateC n t e ue o uo (coerce <$> c) (coerce <$> k)
    Info         n c k             -> InfoC n (coerce <$> c) (coerce <$> k)
    Cleanup      n c k             -> CleanupC n (coerce <$> c) (coerce <$> k)
    Restore      n c k             -> RestoreC n (coerce <$> c) (coerce <$> k)
    CleanArchive c k               ->
      CleanArchiveC (coerce <$> c) (coerce <$> k)
  where
    opts = info (commandArgs <**> helper) fullDesc

commandArgs :: Parser Args
commandArgs =
  subparser (
    command "create" (info createArgs (progDesc "create a new staging"))
    <> command "list" (info listArgs (progDesc "get names all stagings"))
    <> command "delete" (info deleteArgs (progDesc "delete the staging"))
    <> command "update" (info updateArgs (progDesc "update the staging"))
    <> command "info" (info infoArgs (progDesc "get the staging info"))
    <> command "cleanup" (info cleanupArgs (progDesc "cleanup the staging"))
    <> command "restore" (info restoreArgs (progDesc "restore the staging"))
    <> command "clean-archive"
      (info cleanupArchiveArgs (progDesc "cleanup all archived stagings")))

createArgs :: Parser Args
createArgs =
  Create
  <$> strOption (long "name" <> short 'n' <> help "staging name")
  <*> strOption (long "tag" <> short 't' <> help "staging tag")
  <*> many (strOption
    (long "set-app-env-override"
    <> short 'e'
    <> help "application level override"))
  <*> many (strOption
    (long "set-staging-override"
    <> short 'o'
    <> help "staging level override"))
  <*> optional (strOption (long "tlsCertPath" <> help "TLS certificate path"))
  <*> optional (strOption (long "tlsKeyPath" <> help "TLS key path"))

listArgs :: Parser Args
listArgs =
  List
  <$> optional (strOption (long "tlsCertPath" <> help "TLS certificate path"))
  <*> optional (strOption (long "tlsKeyPath" <> help "TLS key path"))

deleteArgs :: Parser Args
deleteArgs =
  Delete
  <$> strOption (long "name" <> short 'n' <> help "staging name")
  <*> optional (strOption (long "tlsCertPath" <> help "TLS certificate path"))
  <*> optional (strOption (long "tlsKeyPath" <> help "TLS key path"))

updateArgs :: Parser Args
updateArgs =
  Update
  <$> strOption (long "name" <> short 'n' <> help "staging name")
  <*> strOption (long "tag" <> short 't' <> help "staging tag")
  <*> many (strOption
    (long "set-app-env-override"
    <> short 'e'
    <> help "application level override"))
  <*> many (strOption
    (long "unset-app-env-override"
    <> short 'E'
    <> help "application level override"))
  <*> many (strOption
    (long "set-staging-override"
    <> short 'o'
    <> help "staging level override"))
  <*> many (strOption
    (long "unset-staging-override"
    <> short 'O'
    <> help "staging level override"))
  <*> optional (strOption (long "tlsCertPath" <> help "TLS certificate path"))
  <*> optional (strOption (long "tlsKeyPath" <> help "TLS key path"))

infoArgs :: Parser Args
infoArgs =
  Info
  <$> strOption (long "name" <> short 'n' <> help "staging name")
  <*> optional (strOption (long "tlsCertPath" <> help "TLS certificate path"))
  <*> optional (strOption (long "tlsKeyPath" <> help "TLS key path"))

cleanupArgs :: Parser Args
cleanupArgs =
  Cleanup
  <$> strOption (long "name" <> short 'n' <> help "staging name")
  <*> optional (strOption (long "tlsCertPath" <> help "TLS certificate path"))
  <*> optional (strOption (long "tlsKeyPath" <> help "TLS key path"))

restoreArgs :: Parser Args
restoreArgs =
  Restore
  <$> strOption (long "name" <> short 'n' <> help "staging name")
  <*> optional (strOption (long "tlsCertPath" <> help "TLS certificate path"))
  <*> optional (strOption (long "tlsKeyPath" <> help "TLS key path"))

cleanupArchiveArgs :: Parser Args
cleanupArchiveArgs =
  CleanArchive
  <$> optional (strOption (long "tlsCertPath" <> help "TLS certificate path"))
  <*> optional (strOption (long "tlsKeyPath" <> help "TLS key path"))
