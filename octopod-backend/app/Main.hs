{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Data.Text as T
import Development.GitRev
import Octopod.Server

main :: IO ()
main = runOctopodServer (T.pack $gitHash)
