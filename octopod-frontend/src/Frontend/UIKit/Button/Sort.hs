module Frontend.UIKit.Button.Sort
  ( sortButton,
    SortButtonConfig (..),
    SortButtonState (..),
  )
where

import Control.Lens
import Data.Default
import Data.Generics.Labels ()
import Data.Text (Text)
import Frontend.Classes
import Frontend.UIKit.Button.Common
import GHC.Generics (Generic)
import Reflex.Dom

data SortButtonConfig t = SortButtonConfig
  { buttonText :: Text
  , buttonEnabled :: Dynamic t Bool
  , buttonState :: Dynamic t (Maybe SortButtonState)
  }
  deriving stock (Generic)

instance Reflex t => Default (SortButtonConfig t) where
  def =
    SortButtonConfig
      { buttonText = ""
      , buttonEnabled = pure True
      , buttonState = pure Nothing
      }

data SortButtonState
  = SortAscButtonState
  | SortDescButtonState

buttonSortStateClasses :: SortButtonState -> Classes
buttonSortStateClasses = \case
  SortAscButtonState -> "sort--active sort--asc"
  SortDescButtonState -> "sort--active sort--desc"

sortButton ::
  (DomBuilder t m, PostBuild t m) =>
  SortButtonConfig t ->
  m (Event t ())
sortButton cfg =
  buttonEl
    CommonButtonConfig
      { constantClasses = do
          state <- cfg ^. #buttonState
          pure $
            "sort"
              <> maybe mempty buttonSortStateClasses state
      , enabledClasses = mempty
      , disabledClasses = mempty
      , buttonEnabled = cfg ^. #buttonEnabled
      , buttonText = pure $ cfg ^. #buttonText
      , buttonBaseTag = ButtonTag
      }
