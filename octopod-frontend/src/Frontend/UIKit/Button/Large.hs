module Frontend.UIKit.Button.Large
  ( largeButton,
    LargeButtonType (..),
    LargeButtonConfig (..),
    LargeButtonStyle (..),
    LargeButtonPriority (..),
    BaseButtonTag (..),
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

data LargeButtonConfig t = LargeButtonConfig
  { buttonText :: Text
  , buttonEnabled :: Dynamic t Bool
  , buttonType :: Dynamic t (Maybe LargeButtonType)
  , buttonPriority :: LargeButtonPriority
  , buttonStyle :: LargeButtonStyle
  , buttonBaseTag :: BaseButtonTag
  }
  deriving stock (Generic)

data LargeButtonPriority = PrimaryLargeButton | SecondaryLargeButton

buttonPriorityClasses :: LargeButtonPriority -> Classes
buttonPriorityClasses PrimaryLargeButton = mempty
buttonPriorityClasses SecondaryLargeButton = "button--secondary"

data LargeButtonStyle
  = RegularLargeButtonStyle
  | PopupActionLargeButtonStyle
  | DialogActionLargeButtonStyle
  | PageActionLargeButtonStyle

buttonStyleClasses :: LargeButtonStyle -> Classes
buttonStyleClasses RegularLargeButtonStyle = mempty
buttonStyleClasses PopupActionLargeButtonStyle = "popup__action"
buttonStyleClasses DialogActionLargeButtonStyle = "dialog__action"
buttonStyleClasses PageActionLargeButtonStyle = "page__action"

instance Reflex t => Default (LargeButtonConfig t) where
  def =
    LargeButtonConfig
      { buttonText = ""
      , buttonEnabled = pure True
      , buttonType = pure Nothing
      , buttonStyle = RegularLargeButtonStyle
      , buttonPriority = PrimaryLargeButton
      , buttonBaseTag = ButtonTag
      }

data LargeButtonType
  = AddLargeButtonType
  | ArchiveLargeButtonType
  | RestoreLargeButtonType
  | EditLargeButtonType
  | LogsLargeButtonType
  | SaveLargeButtonType
  | LoadingLargeButtonType

buttonTypeClasses :: LargeButtonType -> Classes
buttonTypeClasses AddLargeButtonType = "button--add"
buttonTypeClasses ArchiveLargeButtonType = "button--archive"
buttonTypeClasses RestoreLargeButtonType = "button--restore"
buttonTypeClasses EditLargeButtonType = "button--edit"
buttonTypeClasses LogsLargeButtonType = "button--logs"
buttonTypeClasses SaveLargeButtonType = "button--save"
buttonTypeClasses LoadingLargeButtonType = "button--save-loading"

largeButton ::
  (DomBuilder t m, PostBuild t m) =>
  LargeButtonConfig t ->
  m (Event t ())
largeButton cfg =
  buttonEl
    CommonButtonConfig
      { constantClasses = do
          bType <- cfg ^. #buttonType
          pure $
            "button"
              <> maybe mempty buttonTypeClasses bType
              <> buttonStyleClasses (cfg ^. #buttonStyle)
              <> buttonPriorityClasses (cfg ^. #buttonPriority)
      , enabledClasses = mempty
      , disabledClasses = "button--disabled"
      , buttonEnabled = cfg ^. #buttonEnabled
      , buttonText = pure $ cfg ^. #buttonText
      , buttonBaseTag = cfg ^. #buttonBaseTag
      }
