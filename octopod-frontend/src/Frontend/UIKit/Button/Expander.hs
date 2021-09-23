module Frontend.UIKit.Button.Expander
  ( expanderButton,
    ExpanderButtonConfig (..),
    ExpanderButtonStyle (..),
    ExpanderState (..),
    ExpanderButtonType (..),
  )
where

import Control.Lens
import Control.Monad.Fix
import Data.Default
import Data.Generics.Labels ()
import Data.Text (Text)
import Frontend.Classes
import Frontend.UIKit.Button.Common
import GHC.Generics (Generic)
import Reflex.Dom

data ExpanderButtonConfig t = ExpanderButtonConfig
  { buttonText :: Dynamic t Text
  , buttonInitialState :: ExpanderState
  , buttonType :: Maybe ExpanderButtonType
  , buttonStyle :: ExpanderButtonStyle
  }
  deriving stock (Generic)

data ExpanderState = ExpandedState | ContractedState
  deriving stock (Eq)

toggleState :: ExpanderState -> ExpanderState
toggleState ExpandedState = ContractedState
toggleState ContractedState = ExpandedState

expanderButtonStateClasses :: ExpanderState -> Classes
expanderButtonStateClasses ExpandedState = "expander--open"
expanderButtonStateClasses ContractedState = mempty

instance Reflex t => Default (ExpanderButtonConfig t) where
  def =
    ExpanderButtonConfig
      { buttonText = ""
      , buttonInitialState = ContractedState
      , buttonType = Nothing
      , buttonStyle = RegularExpanderButtonStyle
      }

data ExpanderButtonType
  = ListingExpanderButton

buttonTypeClasses :: ExpanderButtonType -> Classes
buttonTypeClasses = \case
  ListingExpanderButton -> "listing__more"

buttonStyleClasses :: ExpanderButtonStyle -> Classes
buttonStyleClasses RegularExpanderButtonStyle = mempty
buttonStyleClasses StandaloneExpanderButtonStyle = "expander--stand-alone"

data ExpanderButtonStyle
  = RegularExpanderButtonStyle
  | StandaloneExpanderButtonStyle

expanderButton ::
  (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) =>
  ExpanderButtonConfig t ->
  m (Dynamic t ExpanderState)
expanderButton cfg = mdo
  stateDyn <- foldDyn (\() -> toggleState) (cfg ^. #buttonInitialState) toggleEv
  let constantClasses =
        "expander"
          <> maybe mempty buttonTypeClasses (cfg ^. #buttonType)
          <> buttonStyleClasses (cfg ^. #buttonStyle)
  toggleEv <-
    buttonEl
      CommonButtonConfig
        { constantClasses = do
            state <- stateDyn
            pure $ constantClasses <> expanderButtonStateClasses state
        , enabledClasses = mempty
        , disabledClasses = mempty
        , buttonEnabled = pure True
        , buttonText = cfg ^. #buttonText
        , buttonBaseTag = ButtonTag
        }
  pure stateDyn
