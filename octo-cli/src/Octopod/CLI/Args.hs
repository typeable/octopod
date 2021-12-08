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
      , -- | application-level overrides to set
        setAppOverrides :: [Text]
      , -- | deployment-level overrides to set
        setDeploymentOverrides :: [Text]
      }
  | List
  | Archive
      { -- | deployment name
        name :: Text
      }
  | Update
      { -- | deployment name
        name :: Text
      , -- | application-level overrides to set
        setAppOverrides :: [Text]
      , -- | application-level overrides to unset
        unsetAppOverrides :: [Text]
      , -- | deployment-level overrides to set
        setDeploymentOverrides :: [Text]
      , -- | deployment-level overrides to unset
        unsetDeploymentOverrides :: [Text]
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
  | GetActionLogs ActionId LogOutput
  | Delete
      { name :: Text
      }
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
          "logs"
          (info actionLogsArgs (progDesc "get deployment logs of a given action"))
        <> command "delete" (info deleteArgs (progDesc "delete the deployment from Octopod. Does no cleanup."))
    )

-- | Parses arguments of 'create' subcommand.
createArgs :: Parser Args
createArgs =
  Create
    <$> deploymentNameArgument
    <*> many
      ( strOption
          ( long "set-app-config"
              <> short 'e'
              <> help "set application level override"
          )
      )
    <*> many
      ( strOption
          ( long "set-deployment-config"
              <> short 'o'
              <> help "set deployment level override"
          )
      )

-- | Parses arguments of 'list' subcommand.
listArgs :: Parser Args
listArgs =
  pure List

-- | Parses arguments of 'archive' subcommand.
archiveArgs :: Parser Args
archiveArgs = Archive <$> deploymentNameArgument

-- | Parses arguments of 'update' subcommand.
updateArgs :: Parser Args
updateArgs =
  Update
    <$> deploymentNameArgument
    <*> many
      ( strOption
          ( long "set-app-config"
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
          ( long "set-deployment-config"
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

-- | Parses arguments of 'info' subcommand.
infoArgs :: Parser Args
infoArgs = Info <$> deploymentNameArgument

-- | Parses arguments of 'cleanup' subcommand.
cleanupArgs :: Parser Args
cleanupArgs = Cleanup <$> deploymentNameArgument

-- | Parses arguments of 'delete' subcommand.
deleteArgs :: Parser Args
deleteArgs = Delete <$> deploymentNameArgument

-- | Parses arguments of 'restore' subcommand.
restoreArgs :: Parser Args
restoreArgs = Restore <$> deploymentNameArgument

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

deploymentNameArgument :: Parser Text
deploymentNameArgument = strOption (long "name" <> short 'n' <> help "deployment name")
