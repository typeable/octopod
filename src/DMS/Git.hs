{-# LANGUAGE OverloadedStrings #-}

module DMS.Git
  ( gitPath
  , upgradeRepo
  , withRepoPath
  ) where

import System.Directory (findExecutable)
import System.Exit
import System.Process.Typed

gitPath :: IO (Maybe FilePath)
gitPath = findExecutable "git"

upgradeRepo :: FilePath -> IO ExitCode
upgradeRepo git = withProcessWait (withRepoPath $ proc git ["pull", "origin", "master"]) waitProcess

withRepoPath :: ProcessConfig i o e -> ProcessConfig i o e
withRepoPath = setWorkingDir repoPath

repoPath = "/b2b-helm"

waitProcess :: (Show stdout, Show stderr) => Process stdin stdout stderr -> IO ExitCode
waitProcess p = do
  print . getStdout $ p
  print . getStderr $ p
  waitExitCode p
