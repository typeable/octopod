module Page.Elements.Links
  ( renderMetadataLink,
  )
where

import Common.Types
import Control.Lens
import qualified Data.Text as T
import Reflex.Dom

renderMetadataLink ::
  (DomBuilder t m, PostBuild t m) =>
  Dynamic t DeploymentMetadatum ->
  m ()
renderMetadataLink metadataD = do
  let attrDyn =
        metadataD <&> \metadata ->
          "class" =: "listing__item external bar bar--larger"
            <> "href" =: metadata ^. #link
            <> "target" =: "_blank"
  elDynAttr "a" attrDyn . dynText $
    metadataD <&> \case
      -- If the name is empty, then use the url
      DeploymentMetadatum {name = name}
        | (not . T.null . T.strip) name -> name
      DeploymentMetadatum {link = url} -> url
