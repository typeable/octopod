module Frontend.UIKit.Button.Static
  ( closeNotificationButton,
    closeClassicPopupButton,
    closePopupButton,
    deleteOverrideButton,
    undoOverrideButton,
  )
where

import Frontend.UIKit.Button.Common
import Reflex.Dom

closeNotificationButton :: (DomBuilder t m, PostBuild t m) => m (Event t (Either () ()))
closeNotificationButton =
  buttonEl
    CommonButtonConfig
      { constantClasses = pure "notification__close"
      , enabledClasses = mempty
      , disabledClasses = mempty
      , buttonEnabled = pure True
      , buttonText = ""
      , buttonBaseTag = ButtonTag
      }

closeClassicPopupButton :: (DomBuilder t m, PostBuild t m) => m (Event t (Either () ()))
closeClassicPopupButton =
  buttonEl
    CommonButtonConfig
      { constantClasses = pure "classic-popup__close"
      , enabledClasses = mempty
      , disabledClasses = mempty
      , buttonEnabled = pure True
      , buttonText = ""
      , buttonBaseTag = ButtonTag
      }

closePopupButton :: (DomBuilder t m, PostBuild t m) => m (Event t (Either () ()))
closePopupButton =
  buttonEl
    CommonButtonConfig
      { constantClasses = pure "popup__close"
      , enabledClasses = mempty
      , disabledClasses = mempty
      , buttonEnabled = pure True
      , buttonText = ""
      , buttonBaseTag = ButtonTag
      }

deleteOverrideButton :: (DomBuilder t m, PostBuild t m) => m (Event t (Either () ()))
deleteOverrideButton =
  buttonEl
    CommonButtonConfig
      { constantClasses = pure "editable-row__delete spot spot--cancel"
      , enabledClasses = mempty
      , disabledClasses = mempty
      , buttonEnabled = pure True
      , buttonText = ""
      , buttonBaseTag = ButtonTag
      }

undoOverrideButton :: (DomBuilder t m, PostBuild t m) => m (Event t (Either () ()))
undoOverrideButton =
  buttonEl
    CommonButtonConfig
      { constantClasses = pure "editable-row__delete spot spot--undo"
      , enabledClasses = mempty
      , disabledClasses = mempty
      , buttonEnabled = pure True
      , buttonText = ""
      , buttonBaseTag = ButtonTag
      }
