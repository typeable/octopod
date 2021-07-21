{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
--Module      : Octopod.CLI.Args
--Description : octo CLI arguments parser utils.
module Octopod.CLI.Args where

import Common.Types
import Data.Text
import Options.Applicative

-- | octo CLI subcommands and arguments definition.
data Args
  = Create
      { -- | deployment name
        name :: Text
      , -- | deployment tag
        tag :: Text
      , -- | application-level overrides to set
        setAppOverrides :: [Text]
      , -- | deployment-level overrides to set
        setDeploymentOverrides :: [Text]
      , -- | application-level private overrides to set
        setAppPrivateOverrides :: [Text]
      , -- | deployment-level private overrides to set
        setDeploymentPrivateOverrides :: [Text]
      }
  | List
  | Archive
      { -- | deployment name
        name :: Text
      }
  | Update
      { -- | deployment name
        name :: Text
      , -- | deployment tag
        tag :: Text
      , -- | application-level overrides to set
        setAppOverrides :: [Text]
      , -- | application-level overrides to unset
        unsetAppOverrides :: [Text]
      , -- | deployment-level overrides to set
        setDeploymentOverrides :: [Text]
      , -- | deployment-level overrides to unset
        unsetDeploymentOverrides :: [Text]
      , -- | application-level private overrides to set
        setAppPrivateOverrides :: [Text]
      , -- | deployment-level private overrides to set
        setDeploymentPrivateOverrides :: [Text]
      }
  | Info
      { -- | deployment name
        name :: Text
      }
  | Cleanup
      { -- | deployment name
        name :: Text
      }
  | Restore
      { -- | deployment name
        name :: Text
      }
  | CleanArchive
  | GetActionLogs ActionId LogOutput
  deriving stock (Show)

data LogOutput = Err | Out | ErrOut
  deriving stock (Show)

readLogOutput :: String -> Maybe LogOutput
readLogOutput "stderr" = Just Err
readLogOutput "stdout" = Just Out
readLogOutput "err" = Just Err
readLogOutput "out" = Just Out
readLogOutput "all" = Just ErrOut
readLogOutput "both" = Just ErrOut
readLogOutput _ = Nothing

-- | Parses octo CLI subcommands and arguments.
parseArgs :: IO Args
parseArgs = execParser $ info (commandArgs <**> helper) fullDesc

-- | Parses octo CLI subcommands.
commandArgs :: Parser Args
commandArgs =
  hsubparser
    ( command "create" (info createArgs (progDesc "create a new deployment"))
        <> command "list" (info listArgs (progDesc "get names all deployments"))
        <> command "archive" (info archiveArgs (progDesc "archive the deployment"))
        <> command "update" (info updateArgs (progDesc "update the deployment"))
        <> command "info" (info infoArgs (progDesc "get the deployment info"))
        <> command "cleanup" (info cleanupArgs (progDesc "cleanup the deployment"))
        <> command "restore" (info restoreArgs (progDesc "restore the deployment"))
        <> command
          "clean-archive"
          (info cleanupArchiveArgs (progDesc "cleanup all archived deployments"))
        <> command
          "logs"
          (info actionLogsArgs (progDesc "get deployment logs of a given action"))
    )

-- | Parses arguments of 'create' subcommand.
createArgs :: Parser Args
createArgs =
  Create
    <$> strOption (long "name" <> short 'n' <> help "deployment name")
    <*> strOption (long "tag" <> short 't' <> help "deployment tag")
    <*> many
      ( strOption
          ( long "set-app-env-override"
              <> short 'e'
              <> help "set application level override"
          )
      )
    <*> many
      ( strOption
          ( long "set-deployment-override"
              <> short 'o'
              <> help "set deployment level override"
          )
      )
    <*> many
      ( strOption
          ( long "set-app-env-private-override"
              <> short 'a'
              <> help "set application level private override"
              <> internal
          )
      )
    <*> many
      ( strOption
          ( long "set-deployment-private-override"
              <> short 's'
              <> help "set deployment level private override"
              <> internal
          )
      )

-- | Parses arguments of 'list' subcommand.
listArgs :: Parser Args
listArgs =
  pure List

-- | Parses arguments of 'archive' subcommand.
archiveArgs :: Parser Args
archiveArgs =
  Archive
    <$> strOption (long "name" <> short 'n' <> help "deployment name")

-- | Parses arguments of 'update' subcommand.
updateArgs :: Parser Args
updateArgs =
  Update
    <$> strOption (long "name" <> short 'n' <> help "deployment name")
    <*> strOption (long "tag" <> short 't' <> help "deployment tag")
    <*> many
      ( strOption
          ( long "set-app-env-override"
              <> short 'e'
              <> help "set application level override"
          )
      )
    <*> many
      ( strOption
          ( long "unset-app-env-override"
              <> short 'E'
              <> help "unset an application level override"
          )
      )
    <*> many
      ( strOption
          ( long "set-deployment-override"
              <> short 'o'
              <> help "set deployment level override"
          )
      )
    <*> many
      ( strOption
          ( long "unset-deployment-override"
              <> short 'O'
              <> help "unset a deployment level override"
          )
      )
    <*> many
      ( strOption
          ( long "set-app-env-private-override"
              <> short 'a'
              <> help "set application level private override"
              <> internal
          )
      )
    <*> many
      ( strOption
          ( long "set-deployment-private-override"
              <> short 's'
              <> help "set deployment level private override"
              <> internal
          )
      )

-- | Parses arguments of 'info' subcommand.
infoArgs :: Parser Args
infoArgs =
  Info
    <$> strOption (long "name" <> short 'n' <> help "deployment name")

-- | Parses arguments of 'cleanup' subcommand.
cleanupArgs :: Parser Args
cleanupArgs =
  Cleanup
    <$> strOption (long "name" <> short 'n' <> help "deployment name")

-- | Parses arguments of 'restore' subcommand.
restoreArgs :: Parser Args
restoreArgs =
  Restore
    <$> strOption (long "name" <> short 'n' <> help "deployment name")

-- | Parses arguments of 'clean-archive' subcommand.
cleanupArchiveArgs :: Parser Args
cleanupArchiveArgs =
  pure CleanArchive

actionLogsArgs :: Parser Args
actionLogsArgs =
  GetActionLogs
    <$> (ActionId <$> option auto (long "action" <> short 'a' <> help "action id"))
    <*> option
      (maybeReader readLogOutput)
      ( long "log-type"
          <> short 'l'
          <> help "log types to print. Values are: stdout, stderr, all"
          <> value ErrOut
          <> completeWith ["stdout", "stderr", "all"]
      )
