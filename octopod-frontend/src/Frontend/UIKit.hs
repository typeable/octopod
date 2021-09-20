-- | Most of HTML details should be implemented here with a domain API.
module Frontend.UIKit
  ( deploymentSection,
    loadingCommonWidget,
    errorCommonWidget,
    octopodTextInput',
    loadingOverride,
    loadingOverrides,
    overrideField,
    OverrideField (..),
    popupOverlay,
    Default (..),
    module X,
    (.~~),
    (?~~),
    OverrideFieldType (..),
    deletedOverride,
    showNonEditableWorkingOverride,
  )
where

import Control.Lens
import Control.Monad
import Data.Align
import Data.Default
import Data.Generics.Labels ()
import qualified Data.Map as M
import Data.Text (Text)
import Data.These
import Data.UniqMap
import Data.WorkingOverrides
import Frontend.Classes as X
import Frontend.UIKit.Button.Action as X
import Frontend.UIKit.Button.Dash as X
import Frontend.UIKit.Button.Expander as X
import Frontend.UIKit.Button.Large as X
import Frontend.UIKit.Button.Sort as X
import Frontend.UIKit.Button.Static as X
import GHC.Generics (Generic)
import Reflex.Dom

(.~~) :: ASetter' s a -> a -> s -> s
(.~~) = (.~)

(?~~) :: ASetter' s (Maybe a) -> a -> s -> s
(?~~) = (?~)

deploymentSection :: DomBuilder t m => Text -> m a -> m a
deploymentSection n m = elClass "section" "deployment__section" $ do
  elClass "h3" "deployment__sub-heading" $ text n
  elDiv "deployment__widget" m

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

data OverrideField t = OverrideField
  { fieldValue :: Dynamic t Text
  , fieldError :: Event t Text
  , fieldDisabled :: Dynamic t Bool
  , fieldType :: Dynamic t OverrideFieldType
  }
  deriving stock (Generic)

data OverrideFieldType
  = DefaultOverrideFieldType
  | EditedOverrideFieldType

overrideFieldTypeClasses :: OverrideFieldType -> Classes
overrideFieldTypeClasses DefaultOverrideFieldType = "input--default"
overrideFieldTypeClasses EditedOverrideFieldType = mempty

overrideField :: MonadWidget t m => OverrideField t -> OverrideField t -> m (Dynamic t Text, Dynamic t Text, Event t ())
overrideField keyDyn valueDyn = do
  elDiv "overrides__item" $ do
    (keyTextDyn, _) <-
      octopodTextInput'
        (keyDyn ^. #fieldDisabled)
        ( do
            t <- keyDyn ^. #fieldType
            pure $ "overrides__key" <> overrideFieldTypeClasses t
        )
        "key"
        (keyDyn ^. #fieldValue)
        (keyDyn ^. #fieldError)
    (valTextDyn, _) <-
      octopodTextInput'
        (valueDyn ^. #fieldDisabled)
        ( do
            t <- keyDyn ^. #fieldType
            pure $ "overrides__value" <> overrideFieldTypeClasses t
        )
        "value"
        (valueDyn ^. #fieldValue)
        (valueDyn ^. #fieldError)
    closeEv <- deleteOverrideButton
    pure (keyTextDyn, valTextDyn, closeEv)

deletedOverride :: MonadWidget t m => Text -> Maybe Text -> m (Event t ())
deletedOverride k vM = do
  elDiv "overrides__item" $ do
    elDiv "overrides__key input input--deleted" $ field k
    case vM of
      Nothing -> do
        elDiv "overrides__placeholder overrides__value" blank
        loadingOverrideSpinner
        pure never
      Just v -> do
        elDiv "overrides__value input input--deleted" $ field $ v
        undoOverrideButton
  where
    field t =
      void $
        inputElement $
          def
            & inputElementConfig_initialValue .~ t
            & initialAttributes .~ ("type" =: "text" <> "class" =: "input__widget")

loadingOverrideField :: MonadWidget t m => m ()
loadingOverrideField = elDiv "overrides__placeholder" blank

loadingOverrideSpinner :: MonadWidget t m => m ()
loadingOverrideSpinner = elDiv "overrides__delete spot spot--loader" blank

loadingOverride :: MonadWidget t m => m ()
loadingOverride = do
  elDiv "overrides__item loader" $ do
    loadingOverrideField
    loadingOverrideField
    loadingOverrideSpinner

loadingOverrides :: MonadWidget t m => m ()
loadingOverrides = do
  loadingOverride
  loadingOverride
  loadingOverride

elDiv :: DomBuilder t m => Text -> m a -> m a
elDiv = elClass "div"

-- | The only text input field that is used in project forms. This input
-- provides automatic error message hiding after user starts typing.
octopodTextInput' ::
  MonadWidget t m =>
  -- | Disabled?
  Dynamic t Bool ->
  -- | Input field classes.
  Dynamic t Classes ->
  -- | Placeholder for input field.
  Text ->
  -- | Init value.
  (Dynamic t Text) ->
  -- | Event carrying the error message.
  Event t Text ->
  m (Dynamic t Text, Dynamic t Bool)
octopodTextInput' disabledDyn clssDyn placeholder inValDyn' errEv = mdo
  inValDyn <- holdUniqDyn inValDyn'
  let inValEv =
        align (updated inValDyn) (updated valDyn)
          & fmapMaybe
            ( \case
                This x -> Just x
                These inV currV | inV /= currV -> Just inV
                _ -> Nothing
            )
  isValid <-
    holdDyn True $
      leftmost
        [ False <$ errEv
        , True <$ updated valDyn
        ]

  errorClassesDyn <-
    holdDyn mempty $
      leftmost
        [ "input--error" <$ errEv
        , mempty <$ updated valDyn
        ]

  let classDyn = do
        errClasses <- errorClassesDyn
        additionalClasses <- clssDyn
        pure . destructClasses $ "input" <> errClasses <> additionalClasses

  inVal <- sample . current $ inValDyn
  disabled <- sample . current $ disabledDyn
  valDyn <- elDynClass "div" classDyn $ do
    inp <-
      inputElement $
        def
          & initialAttributes
            .~ ( "type" =: "text"
                  <> "class" =: "input__widget"
                  <> "placeholder" =: placeholder
               )
          & inputElementConfig_setValue .~ inValEv
          & inputElementConfig_initialValue .~ inVal
          & inputElementConfig_elementConfig . elementConfig_initialAttributes
            %~ (if disabled then M.insert "disabled" "disabled" else id)
          & inputElementConfig_elementConfig . elementConfig_modifyAttributes
            <>~ updated
              ( do
                  disabled' <- disabledDyn
                  pure $
                    M.singleton "disabled" $
                      if disabled' then Just "disabled" else Nothing
              )
    widgetHold_ blank $
      leftmost
        [ divClass "input__output" . text <$> errEv
        , blank <$ updated valDyn
        ]
    pure $ value inp
  pure (valDyn, isValid)

-- | Dark unclickable background for opened sidebar.
popupOverlay :: DomBuilder t m => m ()
popupOverlay =
  elAttr "div" ("class" =: "popup__overlay" <> "aria-hidden" =: "true") blank

-- | Widget that shows overrides list. It does not depend on their type.
showNonEditableWorkingOverride ::
  MonadWidget t m =>
  -- | Overrides list.
  WorkingOverrides ->
  m ()
showNonEditableWorkingOverride (elemsUniq -> cfg) =
  divClass "deployment__widget" $
    divClass "listing listing--for-text listing--larger" $
      forM_ cfg $ \(WorkingOverrideKey keyType key, val) -> do
        let wrapper = case val of
              WorkingDeletedValue _ -> divClass "listing__item deleted"
              _ -> divClass "listing__item"
        wrapper $ do
          let keyWrapper = case keyType of
                CustomWorkingOverrideKey -> elClass "span" "listing__key"
                DefaultWorkingOverrideKey -> elClass "span" "listing__key default"
          keyWrapper $ do
            text key
            text ": "

          case val of
            WorkingCustomValue txt -> elClass "span" "listing__value" $ text txt
            WorkingDefaultValue txt -> elClass "span" "listing__value default" $ text txt
            WorkingDeletedValue (Just txt) -> elClass "span" "listing__value default" $ text txt
            WorkingDeletedValue Nothing -> do
              elClass "div" "listing__placeholder" $ pure ()
              elClass "div" "listing__spinner" $ pure ()
