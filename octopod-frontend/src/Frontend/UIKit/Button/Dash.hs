module Frontend.UIKit.Button.Dash
  ( dashButton,
    DashButtonType (..),
    DashButtonConfig (..),
    DashButtonStyle (..),
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

data DashButtonConfig t = DashButtonConfig
  { buttonText :: Text
  , buttonEnabled :: Dynamic t Bool
  , buttonType :: Maybe DashButtonType
  , buttonStyle :: DashButtonStyle
  }
  deriving stock (Generic)

data DashButtonStyle
  = RegularDashButtonStyle
  | SmallDashButtonStyle
  | OverridesDashButtonStyle

dashButtonStyleClasses :: DashButtonStyle -> Classes
dashButtonStyleClasses RegularDashButtonStyle = mempty
dashButtonStyleClasses SmallDashButtonStyle = "dash--smaller"
dashButtonStyleClasses OverridesDashButtonStyle = "overrides__add"

instance Reflex t => Default (DashButtonConfig t) where
  def =
    DashButtonConfig
      { buttonText = ""
      , buttonEnabled = pure True
      , buttonType = Nothing
      , buttonStyle = RegularDashButtonStyle
      }

data DashButtonType
  = AddDashButtonType
  | BackDashButtonType

buttonTypeClasses :: DashButtonType -> Classes
buttonTypeClasses = \case
  AddDashButtonType -> "dash--add"
  BackDashButtonType -> "dash--back"

dashButton ::
  (DomBuilder t m, PostBuild t m) =>
  DashButtonConfig t ->
  m (Event t ())
dashButton cfg =
  buttonEl
    CommonButtonConfig
      { constantClasses =
          pure $
            "dash"
              <> maybe mempty buttonTypeClasses (cfg ^. #buttonType)
              <> dashButtonStyleClasses (cfg ^. #buttonStyle)
      , enabledClasses = mempty
      , disabledClasses = "dash--disabled"
      , buttonEnabled = cfg ^. #buttonEnabled
      , buttonText = pure $ cfg ^. #buttonText
      , buttonBaseTag = ButtonTag
      }
