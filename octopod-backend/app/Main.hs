{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Development.GitRev
import Octopod.Server
import qualified Data.Text as T

main :: IO ()
main = runOctopodServer (T.pack $gitHash)
