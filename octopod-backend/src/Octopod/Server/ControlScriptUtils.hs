{-|
Module      : Octopod.Server.ControlScriptUtils
Description : Control script utils.

This module contains control script utils.
-}


module Octopod.Server.ControlScriptUtils where


import           Data.Coerce
import           Data.Text


import           Types


-- | Creates command arguments for the 'info' deployment control script.
infoCommandArgs
  :: ProjectName
  -> Domain
  -> Namespace
  -> DeploymentName
  -> ControlScriptArgs
infoCommandArgs pName domain ns dName =
  ControlScriptArgs $
    [ "--project-name", unpack . coerce $ pName
    , "--base-domain", unpack . coerce $ domain
    , "--namespace", unpack . coerce $ ns
    , "--name", unpack . coerce $ dName ]
