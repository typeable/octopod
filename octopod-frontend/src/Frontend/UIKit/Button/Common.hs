module Frontend.UIKit.Button.Common
  ( CommonButtonConfig (..),
    BaseButtonTag (..),
    buttonEl,
    TextBuilder (..),
    textBuilder,
  )
where

import Control.Lens hiding (element)
import Data.Generics.Labels ()
import Data.Map (Map)
import Data.Proxy
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import Frontend.Classes
import GHC.Generics (Generic)
import Reflex.Dom

textBuilder :: Text -> TextBuilder t
textBuilder t = TextBuilder $ text t

newtype TextBuilder t = TextBuilder
  { unTextBuilder ::
      forall m.
      ( DomBuilder t m
      , PostBuild t m
      ) =>
      m ()
  }

instance IsString (TextBuilder t) where
  fromString t = TextBuilder (text . T.pack $ t)

data CommonButtonConfig t = CommonButtonConfig
  { constantClasses :: Dynamic t Classes
  , enabledClasses :: Classes
  , disabledClasses :: Classes
  , buttonEnabled :: Dynamic t Bool
  , buttonText :: TextBuilder t
  , buttonBaseTag :: BaseButtonTag t
  }
  deriving stock (Generic)

data BaseButtonTag t = ButtonTag | ATag (Dynamic t Text)

baseTag :: Reflex t => BaseButtonTag t -> (Text, Dynamic t (Map Text Text))
baseTag ButtonTag = ("button", pure $ "type" =: "button")
baseTag (ATag urlDyn) =
  ( "a"
  , urlDyn <&> \url -> "href" =: url <> "target" =: "_blank"
  )

buttonEl ::
  forall m t.
  (DomBuilder t m, PostBuild t m) =>
  CommonButtonConfig t ->
  m (Event t (Either () ()))
buttonEl cfg = do
  let (t, tagAttrsDyn) = baseTag (cfg ^. #buttonBaseTag)
      attrsDyn = do
        enabled <- cfg ^. #buttonEnabled
        let (enabledClasses, enabledAttrs) = case enabled of
              True -> (cfg ^. #enabledClasses, mempty)
              False -> (cfg ^. #disabledClasses, "disabled" =: "")
        cs <- cfg ^. #constantClasses
        tagAttrs <- tagAttrsDyn
        pure $
          tagAttrs
            <> "class" =: destructClasses (enabledClasses <> cs)
            <> enabledAttrs
  modAttrs <- dynamicAttributesToModifyAttributes attrsDyn
  let elCfg =
        (def @(ElementConfig EventResult t (DomBuilderSpace m)))
          & elementConfig_modifyAttributes .~ fmapCheap mapKeysToAttributeName modAttrs
          & elementConfig_eventSpec
            %~~ addEventSpecFlags
              (Proxy @(DomBuilderSpace m))
              Contextmenu
              ( const
                  EventFlags
                    { _eventFlags_propagation = Propagation_StopImmediate
                    , _eventFlags_preventDefault = False
                    }
              )
          & elementConfig_eventSpec
            %~~ addEventSpecFlags
              (Proxy @(DomBuilderSpace m))
              Click
              ( const
                  EventFlags
                    { _eventFlags_propagation = Propagation_StopImmediate
                    , _eventFlags_preventDefault = False
                    }
              )
  (bEl, _) <- element t elCfg (unTextBuilder (cfg ^. #buttonText))
  pure $ leftmost [Left <$> domEvent Click bEl, Right <$> domEvent Contextmenu bEl]

(%~~) :: ASetter' s a -> (a -> a) -> s -> s
(%~~) = (%~)
