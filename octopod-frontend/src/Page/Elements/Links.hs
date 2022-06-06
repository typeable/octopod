module Page.Elements.Links
  ( renderMetadataLink,
  )
where

import Common.Types
import Common.Utils ((<^.>))
import Control.Lens
import Control.Monad
import qualified Data.Text as T
import Frontend.UIKit.Button.Common
import Reflex.Dom

renderMetadataLink ::
  (DomBuilder t m, PostBuild t m) =>
  Dynamic t DeploymentMetadatum ->
  m ()
renderMetadataLink metadataD = do
  void $
    buttonEl
      CommonButtonConfig
        { constantClasses = pure $ "listing__item" <> "external" <> "bar" <> "bar--larger"
        , enabledClasses = mempty
        , disabledClasses = "button--disabled"
        , buttonEnabled = pure True
        , buttonText =
            TextBuilder $
              dynText $
                metadataD <&> \case
                  -- If the name is empty, then use the url
                  DeploymentMetadatum {name = name}
                    | (not . T.null . T.strip) name -> name
                  DeploymentMetadatum {link = url} -> url
        , buttonBaseTag = ATag $ metadataD <^.> #link
        }
