-- |
--Module      : Page.Popup.NewDeployment
--Description : New deployment sidebar.
--
--This module contains the definition of \"new deployment\" sidebar.
module Page.Popup.NewDeployment (newDeploymentPopup) where

import Control.Lens
import Control.Monad
import Data.Functor
import Data.Generics.Sum
import Data.Monoid
import qualified Data.Text as T
import Reflex.Dom as R
import Prelude as P

import Common.Types
import Common.Validation (isNameValid)
import Frontend.API
import Frontend.Utils
import Servant.Reflex

-- | The root function for \"new deployment\" sidebar.
newDeploymentPopup ::
  MonadWidget t m =>
  -- | \"Show\" event.
  Event t () ->
  -- | \"Close\" event.
  Event t () ->
  m ()
newDeploymentPopup showEv hideEv = void $
  sidebar showEv hideEv $
    const $ mdo
      divClass "popup__body" $ mdo
        (closeEv', saveEv) <- newDeploymentPopupHeader enabledDyn
        (deploymentDyn, validDyn) <- newDeploymentPopupBody respEv
        respEv <- createEndpoint (Right <$> deploymentDyn) saveEv
        sentDyn <-
          holdDyn False $
            leftmost
              [ True <$ saveEv
              , False <$ respEv
              ]
        let successEv =
              fmapMaybe (preview (_Ctor @"Success") <=< commandResponse) respEv
            closeEv = leftmost [closeEv', successEv]
            enabledDyn = zipDynWith (&&) (not <$> sentDyn) validDyn
        pure (never, closeEv)

-- | The header of sidebar contains control buttons: \"Save\" and \"Close\".
newDeploymentPopupHeader ::
  MonadWidget t m =>
  Dynamic t Bool ->
  m (Event t (), Event t ())
newDeploymentPopupHeader enabledDyn =
  divClass "popup__head" $ do
    closeEv <- buttonClass "popup__close" "Close popup"
    elClass "h2" "popup__project" $ text "Create new deployment"
    saveEv <-
      divClass "popup__operations" $
        buttonClassEnabled "popup__action button button--save" "Save" enabledDyn
    divClass "popup__menu drop drop--actions" blank
    pure (closeEv, saveEv)

-- | The body of the sidebar contains the creation form. It contains: a name field,
-- a tag field and overrides fields. The name field is validated with the regexp:
-- @^[a-z][a-z0-9\\-]{1,16}$@.
newDeploymentPopupBody ::
  MonadWidget t m =>
  -- | Request failure event.
  Event t (ReqResult tag CommandResponse) ->
  -- | Returns new deployment and validation states.
  m (Dynamic t Deployment, Dynamic t Bool)
newDeploymentPopupBody errEv = divClass "popup__content" $
  divClass "deployment" $ mdo
    let commandResponseEv = fmapMaybe commandResponse errEv
        nameErrEv = getNameError commandResponseEv nameDyn
    errorHeader err
    (nameDyn, validNameDyn) <- octopodTextInput "tag" "Name" "Name" Nothing nameErrEv
    (du, validDyn, err) <- deploymentPopupBody Nothing mempty mempty errEv
    let dep = do
          du' <- du
          name <- DeploymentName <$> nameDyn
          pure $ Deployment name (du' ^. #newTag) (du' ^. #appOverrides) (du' ^. #deploymentOverrides)
    pure (dep, (&&) <$> validDyn <*> validNameDyn)
  where
    getNameError crEv nameDyn =
      let nameErrEv' = fmapMaybe (preview (_Ctor @"ValidationError" . _1)) crEv
          isNameValidDyn = isNameValid . DeploymentName <$> nameDyn
          badNameText =
            "Deployment name length should be longer than 2 characters \
            \and under 17 characters and begin with a letter."
          badNameEv = badNameText <$ (ffilter not $ updated isNameValidDyn)
          nameErrEv = ffilter (/= "") $ T.intercalate ". " <$> nameErrEv'
       in leftmost [nameErrEv, badNameEv]
