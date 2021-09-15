module Frontend.UIKit
  ( deploymentSection,
    loadingCommonWidget,
    errorCommonWidget,
  )
where

import Data.Text (Text)
import Reflex.Dom

deploymentSection :: DomBuilder t m => Text -> m a -> m a
deploymentSection n m = elClass "section" "deployment__section" $ do
  elClass "h3" "deployment__sub-heading" $ text n
  elClass "div" "deployment__widget" m

-- | Widget with a loading spinner.
loadingCommonWidget :: MonadWidget t m => m ()
loadingCommonWidget =
  divClass "loading loading--enlarged loading--alternate" $
    text "Loading..."

-- | Widget with an error message.
errorCommonWidget :: MonadWidget t m => m ()
errorCommonWidget =
  divClass "null null--data" $
    divClass "null__content" $ do
      elClass "b" "null__heading" $ text "Cannot retrieve the data"
      divClass "null__message" $ text "Try to reload the page"
