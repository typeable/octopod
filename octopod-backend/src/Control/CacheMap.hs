module Control.CacheMap
  ( CacheMap,
    initCacheMap,
    lookupBlocking,
  )
where

import Control.Arrow
import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.Fixed
import Data.IORef.Lifted
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time
import System.Mem.Weak

data CacheMap e c k v = CacheMap !(InternalCacheMap e k v) (forall m. c m => k -> m v) !NominalDiffTime

type InternalCacheMap e k v = IORef (Map k (UTCTime, MVar (Either e v)))

initCacheMap ::
  forall c e k v m.
  MonadBase IO m =>
  -- | Time after which a value is considered "invalid" (removed from cache). Not exact.
  NominalDiffTime ->
  -- | Time after which the value should be updated: upon query the old value would be returned, but
  -- a background computation for the new value would be started.
  NominalDiffTime ->
  (forall n. c n => k -> n v) ->
  m (CacheMap e c k v)
initCacheMap invalid update createValue = do
  ref <- newIORef M.empty
  wRef <- liftBase $ mkWeak ref ref Nothing
  void $ liftBase $ fork $ removeInvalidCaches invalid wRef
  pure $ CacheMap ref createValue update

removeInvalidCaches ::
  -- | Time after which a value is considered invalid. Not exact.
  NominalDiffTime ->
  Weak (InternalCacheMap e k v) ->
  IO ()
removeInvalidCaches invalid wRef = do
  threadDelayMicro (realToFrac $ invalid / 10)
  cutoff <- addUTCTime (negate invalid) <$> getCurrentTime
  deRefWeak wRef >>= \case
    Nothing -> pure ()
    Just ref -> do
      atomicModifyIORef' ref (M.filter (\(t, _) -> t > cutoff) &&& const ())
      removeInvalidCaches invalid wRef
  where
    threadDelayMicro :: Micro -> IO ()
    threadDelayMicro (MkFixed i) = threadDelay (fromInteger i)

lookupBlocking ::
  (MonadBaseControl IO m, MonadError e m, c m, Ord k) =>
  CacheMap e c k v ->
  k ->
  m v
lookupBlocking (CacheMap ref createValue update) k = do
  now <- liftBase getCurrentTime
  blank <- newEmptyMVar
  -- Return the var with an error or result and a bool indicating whether it needs to be updated
  -- If there's no var or if needs to be updated, the update goes into 'blank'
  mVar <- atomicModifyIORef' ref $ \m -> case M.lookup k m of
    Nothing -> (M.insert k (now, blank) m, Nothing)
    Just (timeCreated, var) ->
      if timeCreated < addUTCTime (negate update) now
        then (M.insert k (now, blank) m, Just (var, True))
        else (m, Just (var, False))
  -- Return the var from where the *current* result should be read,
  -- and optionally the var where the recomputation should write to
  (rVar, mwVar) <- case mVar of
    Nothing -> pure (blank, Just blank)
    Just (var, recompute) ->
      takeMVar var >>= \case
        Left _ -> pure $ if recompute then (blank, Just blank) else (var, Just var)
        res@(Right _) -> do
          putMVar var res
          pure $ (var, if recompute then Just blank else Nothing)
  forM_ mwVar $ \wVar -> do
    fork $ do
      v <- (Right <$> createValue k) `catchError` (pure . Left)
      putMVar wVar v
  readMVar rVar >>= liftEither
