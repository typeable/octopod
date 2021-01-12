module Page.Elements.Links
  ( renderMetadataLink
  ) where

import           Common.Types
import           Data.Functor
import qualified Data.Text as T
import           Reflex.Dom

renderMetadataLink
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t DeploymentMetadata -> m ()
renderMetadataLink metadataD = do
  let
    attrDyn = metadataD <&> \metadata ->
      "class" =: "listing__item external bar bar--larger"
      <> "href" =: deploymentMetadataValue metadata
      <> "target" =: "_blank"
  elDynAttr "a" attrDyn . dynText $ metadataD <&> \case
    -- If the name is empty, then use the url
    DeploymentMetadata {deploymentMetadataKey = name}
      | (not . T.null . T.strip) name -> name
    DeploymentMetadata {deploymentMetadataValue = url} -> url
