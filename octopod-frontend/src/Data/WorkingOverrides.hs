module Data.WorkingOverrides
  ( WorkingOverrides,
    WorkingOverrideKey' (..),
    WorkingOverrideKey,
    WorkingOverrideKeyType (..),
    destructWorkingOverrides,
    constructWorkingOverrides,
    ConfigValue (..),
    CustomKey (..),
    CustomConfigValue (..),
    WorkingConfigTree,
    WorkingOverride,
    WorkingOverride',
    configTreeHasLeaf,
    destructWorkingOverridesDyn,
  )
where

import Common.Orphans ()
import Common.Types
import Control.Monad.Reader
import Control.Monad.Ref
import Data.ConfigTree (ConfigTree)
import qualified Data.ConfigTree as CT
import Data.Foldable
import Data.Functor
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import qualified Data.Maybe as M
import Data.Text (Text)
import Data.These
import GHC.Generics (Generic)
import Reflex

type WorkingConfigTree k v = ConfigTree k (ConfigValue v)

data ConfigValue te
  = DefaultConfigValue !te
  | CustomConfigValue !(Either (CustomKey te) (CustomConfigValue te))
  deriving stock (Show, Generic, Eq, Ord)

newtype CustomKey v = CustomKey v
  deriving stock (Show, Generic, Eq, Ord)
data CustomConfigValue te
  = CustomValue !te
  | DeletedValue !(Maybe te)
  deriving stock (Show, Generic, Eq, Ord)

-- Super inefficient but i dont care
configTreeHasLeaf ::
  forall m kv te.
  ( MonadReader (Ref m (Map (ConfigTree kv te) Bool)) m
  , MonadRef m
  , Ord kv
  , Ord te
  ) =>
  (te -> Bool) ->
  ConfigTree kv te ->
  m Bool
configTreeHasLeaf f = memo $ \(CT.ConfigTree x) -> go $ toList x
  where
    go :: [(Maybe te, ConfigTree kv te)] -> m Bool
    go [] = pure False
    go ((Just y, _) : _) | f y = pure True
    go ((_, ct) : rest) =
      configTreeHasLeaf f ct >>= \case
        True -> pure True
        False -> go rest

memo ::
  (MonadReader (Ref m (Map x y)) m, Ord x, MonadRef m) =>
  (x -> m y) ->
  x ->
  m y
memo f x = do
  memoRef <- ask
  m <- readRef memoRef
  case M.lookup x m of
    Just y -> pure y
    Nothing -> do
      y <- f x
      modifyRef memoRef $ M.insert x y
      pure y
type WorkingOverrides = WorkingOverrides' Text

type WorkingOverrides' te = ConfigTree te (ConfigValue te)

type WorkingOverride = WorkingOverride' Text

type WorkingOverride' te = (te, ConfigValue te)

type WorkingOverrideKey = WorkingOverrideKey' Text

data WorkingOverrideKey' te = WorkingOverrideKey !WorkingOverrideKeyType !te
  deriving stock (Show)

data WorkingOverrideKeyType = CustomWorkingOverrideKey | DefaultWorkingOverrideKey
  deriving stock (Show, Eq)

destructWorkingOverridesDyn :: Reflex t => [Dynamic t WorkingOverride] -> Dynamic t (Overrides l)
destructWorkingOverridesDyn =
  fmap (Overrides . CT.fromFlatList . catMaybes) . sequenceA . (fmap . fmap) (\(k, v) -> (k,) <$> unConfigValue v)

destructWorkingOverrides :: [WorkingOverride] -> Overrides l
destructWorkingOverrides =
  Overrides . CT.fromFlatList . M.catMaybes . fmap (\(k, v) -> (k,) <$> unConfigValue v)

unConfigValue :: ConfigValue te -> Maybe (OverrideValue' te)
unConfigValue (DefaultConfigValue _) = Nothing
unConfigValue (CustomConfigValue (Left (CustomKey te))) = Just $ ValueAdded te
unConfigValue (CustomConfigValue (Right (CustomValue te))) = Just $ ValueAdded te
unConfigValue (CustomConfigValue (Right (DeletedValue _))) = Just ValueDeleted

constructWorkingOverrides ::
  Ord te =>
  DefaultConfig' te l ->
  Overrides' te l ->
  WorkingOverrides' te
constructWorkingOverrides (DefaultConfig defCT) (Overrides newCT) =
  CT.catMaybes $
    CT.zip newCT defCT <&> \case
      That x -> Just $ DefaultConfigValue x
      This x -> case x of
        ValueDeleted -> Nothing
        ValueAdded v -> Just $ CustomConfigValue $ Left $ CustomKey v
      These ValueDeleted v -> Just $ CustomConfigValue $ Right $ DeletedValue $ Just v
      These (ValueAdded v) _ -> Just $ CustomConfigValue $ Right $ CustomValue v
