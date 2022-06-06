module Frontend.UIKit.Button.Action
  ( actionButton,
    ActionButtonConfig (..),
    ActionButtonType (..),
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

data ActionButtonConfig t = ActionButtonConfig
  { buttonText :: Text
  , buttonEnabled :: Dynamic t Bool
  , buttonType :: Maybe ActionButtonType
  , buttonBaseTag :: BaseButtonTag t
  }
  deriving stock (Generic)

instance Reflex t => Default (ActionButtonConfig t) where
  def =
    ActionButtonConfig
      { buttonText = ""
      , buttonEnabled = pure True
      , buttonType = Nothing
      , buttonBaseTag = ButtonTag
      }

data ActionButtonType
  = ArchiveActionButtonType
  | EditActionButtonType
  | RestoreActionButtonType
  | LogsActionButtonType

buttonTypeClasses :: ActionButtonType -> Classes
buttonTypeClasses = \case
  ArchiveActionButtonType -> "action--archive"
  EditActionButtonType -> "action--edit"
  RestoreActionButtonType -> "action--restore"
  LogsActionButtonType -> "action--logs"

actionButton ::
  (DomBuilder t m, PostBuild t m) =>
  ActionButtonConfig t ->
  m (Event t (Either () ()))
actionButton cfg =
  buttonEl
    CommonButtonConfig
      { constantClasses =
          pure $
            "action"
              <> maybe mempty buttonTypeClasses (cfg ^. #buttonType)
      , enabledClasses = mempty
      , disabledClasses = "action--disabled"
      , buttonEnabled = cfg ^. #buttonEnabled
      , buttonText = textBuilder $ cfg ^. #buttonText
      , buttonBaseTag = cfg ^. #buttonBaseTag
      }
