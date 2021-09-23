module Frontend.UIKit.Button.Common
  ( CommonButtonConfig (..),
    BaseButtonTag (..),
    buttonEl,
  )
where

import Control.Lens
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Text (Text)
import Frontend.Classes
import GHC.Generics (Generic)
import Reflex.Dom

data CommonButtonConfig t = CommonButtonConfig
  { constantClasses :: Dynamic t Classes
  , enabledClasses :: Classes
  , disabledClasses :: Classes
  , buttonEnabled :: Dynamic t Bool
  , buttonText :: Dynamic t Text
  , buttonBaseTag :: BaseButtonTag
  }
  deriving stock (Generic)

data BaseButtonTag = ButtonTag | ATag Text

baseTag :: BaseButtonTag -> (Text, Map Text Text)
baseTag ButtonTag = ("button", "type" =: "button")
baseTag (ATag url) = ("a", "href" =: url <> "target" =: "_blank")

buttonEl ::
  (DomBuilder t m, PostBuild t m) =>
  CommonButtonConfig t ->
  m (Event t ())
buttonEl cfg = do
  let (t, staticAttrs) = baseTag (cfg ^. #buttonBaseTag)
      attrsDyn = do
        enabled <- cfg ^. #buttonEnabled
        let (enabledClasses, enabledAttrs) = case enabled of
              True -> (cfg ^. #enabledClasses, mempty)
              False -> (cfg ^. #disabledClasses, "disabled" =: "")
        cs <- cfg ^. #constantClasses
        pure $
          staticAttrs
            <> "class" =: destructClasses (enabledClasses <> cs)
            <> enabledAttrs
  (bEl, _) <- elDynAttr' t attrsDyn $ dynText $ cfg ^. #buttonText
  pure $ domEvent Click bEl
