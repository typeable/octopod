module Frontend.Classes
  ( Classes,
    destructClasses,
  )
where

import Data.String
import Data.Text (Text)
import qualified Data.Text as T

newtype Classes = Classes [Text]
  deriving newtype (Semigroup, Monoid)

instance IsString Classes where
  fromString s = Classes [fromString s]

destructClasses :: Classes -> Text
destructClasses (Classes cs) = T.unwords cs
