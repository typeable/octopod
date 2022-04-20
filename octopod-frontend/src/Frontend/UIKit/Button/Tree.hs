{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
module Frontend.UIKit.Button.Tree
  ( treeButton,
    TreeButtonConfig (..),
    TextBuilder (..),
    TreeButtonStatus (..),
    TreeButtonApply (..),
  )
where

import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Data.Default
import Data.Functor
import Data.Generics.Labels ()
import Frontend.UIKit.Button.Common
import GHC.Generics (Generic)
import Reflex.Dom
import Reflex.Network (networkView)

data TreeButtonConfig t = TreeButtonConfig
  { buttonText :: TextBuilder t
  , subtreeHasChanges :: Bool
  , visible :: Dynamic t Bool
  , -- | From outside
    forceState :: Event t Bool
  }
  deriving stock (Generic)

instance Reflex t => Default (TreeButtonConfig t) where
  def =
    TreeButtonConfig
      { buttonText = ""
      , subtreeHasChanges = False
      , visible = pure True
      , forceState = never
      }

treeButton ::
  (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) =>
  TreeButtonConfig t ->
  -- | (<Open?>, <Click>)
  m (Event t (Bool, Either () ()))
treeButton cfg = mdo
  (traceEvent "treeButton" -> clickedEv) <-
    networkView >=> switchHold never $
      (cfg ^. #visible) <&> \visible ->
        if visible
          then
            buttonEl
              CommonButtonConfig
                { constantClasses = do
                    open <- openDyn
                    pure $
                      "collapse__head"
                        <> (if cfg ^. #subtreeHasChanges then "collapse__head--has-changes" else mempty)
                        <> (if open then mempty else "collapse--expanded")
                , buttonText = cfg ^. #buttonText
                , buttonBaseTag = ButtonTag
                , buttonEnabled = pure True
                , disabledClasses = mempty
                , enabledClasses = mempty
                }
          else pure never
  openDyn <-
    foldDyn
      ( \case
          Left () -> not
          Right x -> const x
      )
      False
      $ (leftmost [Right <$> cfg ^. #forceState, clickedEv $> Left ()])
  pure $ coincidence $ (\click -> (,click) <$> updated openDyn) <$> clickedEv

data TreeButtonStatus
  = TreeButtonClosed
  | TreeButtonOpen

data TreeButtonApply
  = TreeButtonApplyToSelf
  | TreeButtonApplyToSubtree
