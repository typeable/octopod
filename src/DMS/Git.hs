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
cloneRepo git repoPath =
  withProcessWait (setWorkingDir repoPath $ proc git args) waitProcess
  where
    args =
      [ "clone"
      , "--recursive"
      , "--depth=1"
      , "git@github.com:Aviora/b2b-helm.git"
      , "." ]

waitProcess :: (Show o, Show e) => Process i o e -> IO ExitCode
waitProcess p = do
  print . getStdout $ p
  print . getStderr $ p
  waitExitCode p
