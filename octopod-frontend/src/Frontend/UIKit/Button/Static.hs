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

closeNotificationButton :: (DomBuilder t m, PostBuild t m) => m (Event t ())
closeNotificationButton =
  buttonEl
    CommonButtonConfig
      { constantClasses = pure "notification__close"
      , enabledClasses = mempty
      , disabledClasses = mempty
      , buttonEnabled = pure True
      , buttonText = pure ""
      , buttonBaseTag = ButtonTag
      }

closeClassicPopupButton :: (DomBuilder t m, PostBuild t m) => m (Event t ())
closeClassicPopupButton =
  buttonEl
    CommonButtonConfig
      { constantClasses = pure "classic-popup__close"
      , enabledClasses = mempty
      , disabledClasses = mempty
      , buttonEnabled = pure True
      , buttonText = pure ""
      , buttonBaseTag = ButtonTag
      }

closePopupButton :: (DomBuilder t m, PostBuild t m) => m (Event t ())
closePopupButton =
  buttonEl
    CommonButtonConfig
      { constantClasses = pure "popup__close"
      , enabledClasses = mempty
      , disabledClasses = mempty
      , buttonEnabled = pure True
      , buttonText = pure ""
      , buttonBaseTag = ButtonTag
      }

deleteOverrideButton :: (DomBuilder t m, PostBuild t m) => m (Event t ())
deleteOverrideButton =
  buttonEl
    CommonButtonConfig
      { constantClasses = pure "overrides__delete spot spot--cancel"
      , enabledClasses = mempty
      , disabledClasses = mempty
      , buttonEnabled = pure True
      , buttonText = pure ""
      , buttonBaseTag = ButtonTag
      }

undoOverrideButton :: (DomBuilder t m, PostBuild t m) => m (Event t ())
undoOverrideButton =
  buttonEl
    CommonButtonConfig
      { constantClasses = pure "overrides__delete spot spot--undo"
      , enabledClasses = mempty
      , disabledClasses = mempty
      , buttonEnabled = pure True
      , buttonText = pure ""
      , buttonBaseTag = ButtonTag
      }
