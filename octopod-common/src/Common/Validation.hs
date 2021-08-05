-- |
--Module      : Common.Validation
--Description : Common validations for backend and frontend.
--
--This module contains common validations between the backend and the frontend.
module Common.Validation
  ( isNameValid,
  )
where

import Common.Types
import Data.Char
import qualified Data.Text as T

-- | Validates a deployment name.
isNameValid :: DeploymentName -> Bool
isNameValid (DeploymentName (T.uncons -> Just (n, nn))) =
  let l = T.length nn
   in l > 0 && l < 16 && isAsciiLower n && T.all (\c -> c == '-' || isAsciiLower c || isDigit c) nn
isNameValid _ = False
