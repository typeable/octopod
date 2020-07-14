module Common.Validation where

import Data.ByteString (ByteString)
import Data.Text as T (Text)
import Data.Text.Encoding as T (encodeUtf8)
import Text.Regex.TDFA

import Common.Types

isNameValid :: DeploymentName -> Bool
isNameValid (DeploymentName name) =
  (T.encodeUtf8 name =~ ("^[a-z][a-z0-9\\-]{1,16}$" :: ByteString))
