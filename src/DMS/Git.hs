{-# LANGUAGE OverloadedStrings #-}

module DMS.Git
  ( gitPath
  , cloneRepo
  ) where

import System.Directory (findExecutable)
import System.Exit
import System.Process.Typed

gitPath :: IO (Maybe FilePath)
gitPath = findExecutable "git"

cloneRepo :: FilePath -> FilePath -> IO ExitCode
cloneRepo git repoPath = withProcessWait (setWorkingDir repoPath $ proc git args) waitProcess
  where args = ["clone", "--recursive", "--depth=1", "git@github.com:Aviora/b2b-helm.git", "."]

waitProcess :: (Show stdout, Show stderr) => Process stdin stdout stderr -> IO ExitCode
waitProcess p = do
  print . getStdout $ p
  print . getStderr $ p
  waitExitCode p
