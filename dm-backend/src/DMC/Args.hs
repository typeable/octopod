module DMC.Args where

import Data.ByteString (ByteString)
import Data.Coerce
import Data.Semigroup ((<>))
import Data.Text
import Options.Applicative


import Types


data Args
  = Create
    { name                      :: Text
    , tag                       :: Text
    , setAppOverrides           :: [Text]
    , setStagingOverrides       :: [Text]
    , setAppSecretOverrides     :: [Text]
    , setStagingSecretOverrides :: [Text]
    , tlsCertPath               :: Maybe ByteString
    , tlsKeyPath                :: Maybe ByteString }
  | List
    { tlsCertPath               :: Maybe ByteString
    , tlsKeyPath                :: Maybe ByteString }
  | Delete
    { name                      :: Text
    , tlsCertPath               :: Maybe ByteString
    , tlsKeyPath                :: Maybe ByteString }
  | Update
    { name                      :: Text
    , tag                       :: Text
    , setAppOverrides           :: [Text]
    , unsetAppOverrides         :: [Text]
    , setStagingOverrides       :: [Text]
    , unsetStagingOverrides     :: [Text]
    , setAppSecretOverrides     :: [Text]
    , setStagingSecretOverrides :: [Text]
    , tlsCertPath               :: Maybe ByteString
    , tlsKeyPath                :: Maybe ByteString }
  | Info
    { name                      :: Text
    , tlsCertPath               :: Maybe ByteString
    , tlsKeyPath                :: Maybe ByteString }
  | Cleanup
    { name                      :: Text
    , tlsCertPath               :: Maybe ByteString
    , tlsKeyPath                :: Maybe ByteString }
  | Restore
    { name                      :: Text
    , tlsCertPath               :: Maybe ByteString
    , tlsKeyPath                :: Maybe ByteString }
  | CleanArchive
    { tlsCertPath               :: Maybe ByteString
    , tlsKeyPath                :: Maybe ByteString }
  deriving stock (Show)

data DMCArgs
  = CreateC
    { deploymentName                     :: Text
    , deploymentTag                      :: Text
    , deploymentSetAppOverrides          :: [Text]
    , deploymentSetStaingOverrides       :: [Text]
    , deploymentSetAppSecretOverrides    :: [Text]
    , deploymentSetStaingSecretOverrides :: [Text]
    , dmcTLSCertPath                     :: Maybe TLSCertPath
    , dmcTLSKeyPath                      :: Maybe TLSKeyPath }
  | ListC
    { dmcTLSCertPath                     :: Maybe TLSCertPath
    , dmcTLSKeyPath                      :: Maybe TLSKeyPath }
  | DeleteC
    { deploymentName                     :: Text
    , dmcTLSCertPath                     :: Maybe TLSCertPath
    , dmcTLSKeyPath                      :: Maybe TLSKeyPath }
  | UpdateC
    { deploymentName                     :: Text
    , deploymentTag                      :: Text
    , deploymentSetAppOverrides          :: [Text]
    , deploymentUnsetAppOverrides        :: [Text]
    , deploymentSetStaingOverrides       :: [Text]
    , deploymentUnsetStaingOverrides     :: [Text]
    , deploymentSetAppSecretOverrides    :: [Text]
    , deploymentSetStaingSecretOverrides :: [Text]
    , dmcTLSCertPath                     :: Maybe TLSCertPath
    , dmcTLSKeyPath                      :: Maybe TLSKeyPath }
  | InfoC
    { deploymentName                     :: Text
    , dmcTLSCertPath                     :: Maybe TLSCertPath
    , dmcTLSKeyPath                      :: Maybe TLSKeyPath }
  | CleanupC
    { deploymentName                     :: Text
    , dmcTLSCertPath                     :: Maybe TLSCertPath
    , dmcTLSKeyPath                      :: Maybe TLSKeyPath }
  | RestoreC
    { deploymentName                     :: Text
    , dmcTLSCertPath                     :: Maybe TLSCertPath
    , dmcTLSKeyPath                      :: Maybe TLSKeyPath }
  | CleanArchiveC
    { dmcTLSCertPath                     :: Maybe TLSCertPath
    , dmcTLSKeyPath                      :: Maybe TLSKeyPath }
  deriving stock (Show)

parseArgs :: IO DMCArgs
parseArgs = do
  args <- execParser opts
  pure $ case args of
    Create       n t e o se so c k ->
      CreateC n t e o se so (coerce <$> c) (coerce <$> k)
    List         c k                     ->
      ListC (coerce <$> c) (coerce <$> k)
    Delete       n c k                   ->
      DeleteC n (coerce <$> c) (coerce <$> k)
    Update       n t e ue o uo se so c k ->
      UpdateC n t e ue o uo se so (coerce <$> c) (coerce <$> k)
    Info         n c k                   ->
      InfoC n (coerce <$> c) (coerce <$> k)
    Cleanup      n c k                   ->
      CleanupC n (coerce <$> c) (coerce <$> k)
    Restore      n c k                   ->
      RestoreC n (coerce <$> c) (coerce <$> k)
    CleanArchive c k                     ->
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
    <> help "set application level public override"))
  <*> many (strOption
    (long "set-staging-override"
    <> short 'o'
    <> help "set staging level public override"))
  <*> many (strOption
    (long "set-app-env-secret-override"
    <> short 'a'
    <> help "set application level secret override"))
  <*> many (strOption
    (long "set-staging-secret-override"
    <> short 's'
    <> help "set staging level secret override"))
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
    <> help "set application level public override"))
  <*> many (strOption
    (long "unset-app-env-override"
    <> short 'E'
    <> help "unset application level public or private override"))
  <*> many (strOption
    (long "set-staging-override"
    <> short 'o'
    <> help "set staging level public override"))
  <*> many (strOption
    (long "unset-staging-override"
    <> short 'O'
    <> help "unset staging level public or private override"))
  <*> many (strOption
    (long "set-app-env-secret-override"
    <> short 'a'
    <> help "set application level secret override"))
  <*> many (strOption
    (long "set-staging-secret-override"
    <> short 's'
    <> help "set staging level secret override"))
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
