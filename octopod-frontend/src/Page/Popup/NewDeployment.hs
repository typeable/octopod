-- |
--Module      : Page.Popup.NewDeployment
--Description : New deployment sidebar.
--
--This module contains the definition of \"new deployment\" sidebar.
module Page.Popup.NewDeployment (newDeploymentPopup) where

import Control.Lens
import Control.Monad
import Data.Functor
import Data.Monoid
import qualified Data.Text as T
import Reflex.Dom as R
import Prelude as P

import Common.Types
import Common.Validation (isNameValid)
import Data.Maybe
import Frontend.API
import Frontend.UIKit
import Frontend.Utils
import Reflex.Network
import Servant.Reflex

-- | The root function for \"new deployment\" sidebar.
newDeploymentPopup ::
  MonadWidget t m =>
  -- | \"Show\" event.
  Event t () ->
  -- | \"Close\" event.
  Event t () ->
  m (Event t Deployment)
newDeploymentPopup showEv hideEv =
  catchReturns $ \enterEv ->
    sidebar showEv hideEv $
      const $ mdo
        divClass "popup__body" $ mdo
          (closeEv', (enterEv <>) -> saveEv) <- newDeploymentPopupHeader enabledDyn sentDyn
          deploymentMDyn <- newDeploymentPopupBody (snd <$> respEv)
          respEv <-
            holdDyn (pure never) >=> networkView >=> switchHold never $
              tagMaybe (current deploymentMDyn) saveEv <&> \dep -> do
                pb <- getPostBuild
                fmap (dep,)
                  <$> createEndpoint
                    (pure $ Right dep)
                    pb
          sentDyn <-
            holdDyn False $
              leftmost
                [ True <$ saveEv
                , False <$ respEv
                ]
          let successEv =
                respEv `fforMaybe` \case
                  (dep, commandResponse -> Just Success {}) -> Just dep
                  _ -> Nothing
              closeEv = leftmost [closeEv', successEv $> ()]
              enabledDyn = zipDynWith (&&) (not <$> sentDyn) (isJust <$> deploymentMDyn)
          pure (successEv, closeEv)

-- | The header of sidebar contains control buttons: \"Save\" and \"Close\".
newDeploymentPopupHeader ::
  MonadWidget t m =>
  Dynamic t Bool ->
  -- | Loading
  Dynamic t Bool ->
  m (Event t (), Event t ())
newDeploymentPopupHeader enabledDyn loadingDyn =
  divClass "popup__head" $ do
    closeEv <- closePopupButton
    elClass "h2" "popup__project" $ text "Create new deployment"
    saveEv <-
      divClass "popup__operations" $
        largeButton $
          def
            & #buttonStyle .~~ PopupActionLargeButtonStyle
            & #buttonText .~~ "Save"
            & #buttonEnabled .~~ enabledDyn
            & #buttonType
              .~~ ( loadingDyn <&> \case
                      False -> Just SaveLargeButtonType
                      True -> Just LoadingLargeButtonType
                  )
    divClass "popup__menu drop drop--actions" blank
    pure (closeEv $> (), saveEv $> ())

-- | The body of the sidebar contains the creation form. It contains: a name field,
-- a tag field and overrides fields. The name field is validated with the regexp:
-- @^[a-z][a-z0-9\\-]{1,16}$@.
newDeploymentPopupBody ::
  MonadWidget t m =>
  -- | Request failure event.
  Event t (ReqResult tag CommandResponse) ->
  -- | Returns new deployment and validation states.
  m (Dynamic t (Maybe Deployment))
newDeploymentPopupBody errEv = divClass "popup__content" $
  divClass "deployment" $
    wrapRequestErrors $ \hReq -> mdo
      let commandResponseEv = fmapMaybe commandResponse errEv
          nameErrEv = getNameError commandResponseEv nameDyn
      (nameDyn, validNameDyn) <- octopodTextInput "tag" "Name" "Name" Nothing nameErrEv
      depDyn <- deploymentPopupBody hReq mempty mempty errEv
      let dep = do
            depDyn >>= \case
              Nothing -> pure Nothing
              Just du' -> do
                name <- DeploymentName <$> nameDyn
                pure . Just $ Deployment name (du' ^. #appOverrides) (du' ^. #deploymentOverrides)
      pure $ do
        validNameDyn >>= \case
          False -> pure Nothing
          True -> dep
  where
    getNameError crEv nameDyn =
      let nameErrEv' =
            fmapMaybe
              ( \case
                  ValidationError nameErr -> Just nameErr
                  _ -> Nothing
              )
              crEv
          isNameValidDyn = isNameValid . DeploymentName <$> nameDyn
          badNameText =
            "Deployment name length should be longer than 2 characters \
            \and under 17 characters and begin with a letter."
          badNameEv = badNameText <$ (ffilter not $ updated isNameValidDyn)
          nameErrEv = ffilter (/= "") $ T.intercalate ". " <$> nameErrEv'
       in leftmost [nameErrEv, badNameEv]
