module Data.WorkingOverrides
  ( WorkingOverrides,
    WorkingOverride,
    WorkingOverride',
    WorkingOverrideKey' (..),
    WorkingOverrideKey,
    WorkingOverrideKeyType (..),
    WorkingOverrideValue' (..),
    WorkingOverrideValue,
    destructWorkingOverrides,
    constructWorkingOverrides,
    newWorkingOverride,
  )
where

import Common.Types
import qualified Data.List as L
import qualified Data.Map.Ordered.Strict as OM
import Data.Maybe
import Data.Text (Text)
import Data.UniqMap

type WorkingOverrides = WorkingOverrides' Text

type WorkingOverrides' te = UniqKeyMap (WorkingOverride' te)

type WorkingOverride = WorkingOverride' Text

type WorkingOverride' te = (WorkingOverrideKey' te, WorkingOverrideValue' te)

type WorkingOverrideKey = WorkingOverrideKey' Text

data WorkingOverrideKey' te = WorkingOverrideKey !WorkingOverrideKeyType !te
  deriving stock (Show)

data WorkingOverrideKeyType = CustomWorkingOverrideKey | DefaultWorkingOverrideKey
  deriving stock (Show, Eq)

type WorkingOverrideValue = WorkingOverrideValue' Text

data WorkingOverrideValue' te
  = WorkingCustomValue !te
  | WorkingDefaultValue !te
  | WorkingDeletedValue !(Maybe te)
  deriving stock (Show)

destructWorkingOverrides :: WorkingOverrides -> Overrides l
destructWorkingOverrides =
  Overrides
    . OM.fromList
    . mapMaybe
      ( \case
          (WorkingOverrideKey CustomWorkingOverrideKey k, getWorkingOverrideValue -> v) -> Just (k, v)
          (WorkingOverrideKey _ k, WorkingCustomValue v) -> Just (k, ValueAdded v)
          (WorkingOverrideKey _ k, WorkingDeletedValue _) -> Just (k, ValueDeleted)
          (WorkingOverrideKey DefaultWorkingOverrideKey _, WorkingDefaultValue _) -> Nothing
      )
    . elemsUniq
  where
    getWorkingOverrideValue :: WorkingOverrideValue -> OverrideValue
    getWorkingOverrideValue (WorkingCustomValue x) = ValueAdded x
    getWorkingOverrideValue (WorkingDefaultValue x) = ValueAdded x
    getWorkingOverrideValue (WorkingDeletedValue _) = ValueDeleted

constructWorkingOverrides ::
  Ord te =>
  Maybe (DefaultConfig' te l) ->
  Overrides' te l ->
  WorkingOverrides' te
constructWorkingOverrides (Just (DefaultConfig dCfg)) (Overrides ovsM) =
  let custom =
        uniqMapFromList
          . mapMaybe
            ( \(k, v) ->
                let k' = WorkingOverrideKey (if OM.member k dCfg then DefaultWorkingOverrideKey else CustomWorkingOverrideKey) k
                 in case v of
                      ValueAdded x -> Just (k', WorkingCustomValue x)
                      ValueDeleted -> (k',) . WorkingDeletedValue . Just <$> OM.lookup k dCfg
            )
          . OM.assocs
          $ ovsM
   in L.foldl'
        ( \m (k, v) ->
            if OM.member k ovsM
              then m
              else
                fst $
                  insertUniqEnd (WorkingOverrideKey DefaultWorkingOverrideKey k, WorkingDefaultValue v) m
        )
        custom
        (OM.assocs dCfg)
constructWorkingOverrides Nothing (Overrides ovsM) =
  uniqMapFromList
    . fmap
      ( \(k, v) ->
          let k' = WorkingOverrideKey CustomWorkingOverrideKey k
           in case v of
                ValueAdded x -> (k', WorkingCustomValue x)
                ValueDeleted -> (k', WorkingDeletedValue Nothing)
      )
    . OM.assocs
    $ ovsM

newWorkingOverride :: WorkingOverride
newWorkingOverride = (WorkingOverrideKey CustomWorkingOverrideKey "", WorkingCustomValue "")
