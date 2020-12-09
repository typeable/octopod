{-|
Module      : Common.Validation
Description : Common validations for backend and frontend.

This module contains common validations between the backend and the frontend.
-}


module Common.Validation where

import           Data.ByteString (ByteString)
import           Data.Text.Encoding as T (encodeUtf8)
import           Text.Regex.TDFA

import           Common.Types

-- | Validates a deployment name.
isNameValid :: DeploymentName -> Bool
isNameValid (DeploymentName name) =
  (T.encodeUtf8 name =~ ("^[a-z][a-z0-9\\-]{1,16}$" :: ByteString))
