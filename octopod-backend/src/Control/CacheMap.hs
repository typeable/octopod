module Control.CacheMap
  ( CacheMap,
    initCacheMap,
    lookupBlocking,
    ntCacheMap,
  )
where

import Control.Concurrent.Lifted
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.Fixed
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time
import System.Mem.Weak

data CacheMap e m k v = CacheMap !(InternalCacheMap e k v) !(k -> m v) !NominalDiffTime

type InternalCacheMap e k v = TVar (Map k (UTCTime, TMVar (Either e v)))

ntCacheMap :: (forall x. m x -> n x) -> CacheMap e m k v -> CacheMap e n k v
ntCacheMap f (CacheMap m g t) = CacheMap m (f <$> g) t

initCacheMap ::
  -- | Time after which a value is considered invalid. Not exact.
  NominalDiffTime ->
  -- | Time after which the value should be updated.
  -- (A value can still be valid and need updating – the caller will get the
  -- old valid value in that case.)
  NominalDiffTime ->
  (k -> m v) ->
  IO (CacheMap e m k v)
initCacheMap invalid update createValue = do
  ref <- newTVarIO M.empty
  wRef <- mkWeakTVar ref (pure ())
  void $ fork $ removeInvalidCaches invalid wRef
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
      atomically $ modifyTVar' ref $ M.filter (\(t, _) -> t > cutoff)
      removeInvalidCaches invalid wRef
  where
    threadDelayMicro :: Micro -> IO ()
    threadDelayMicro (MkFixed i) = threadDelay (fromInteger i)

lookupBlocking ::
  (MonadBaseControl IO m, MonadError e m, Ord k) =>
  CacheMap e m k v ->
  k ->
  m v
lookupBlocking (CacheMap mTVar createValue update) k = do
  now <- liftBase getCurrentTime
  (vM, recreate) <-
    liftBase $
      atomically $ do
        m <- readTVar mTVar
        case M.lookup k m of
          Nothing -> do
            blank <- newEmptyTMVar
            writeTVar mTVar $ M.insert k (now, blank) m
            pure (blank, True)
          Just (timeCreated, vM) -> do
            tryReadTMVar vM >>= \case
              Just (Left _) -> do
                void $ takeTMVar vM
                pure (vM, True)
              _ -> pure (vM, timeCreated < addUTCTime (negate update) now)
  when recreate $
    void $
      fork $ do
        v' <- (Right <$> createValue k) `catchError` (pure . Left)
        replaceTMVar vM v'

  (liftBase . atomically . readTMVar) vM >>= liftEither

replaceTMVar :: MonadBase IO m => TMVar a -> a -> m ()
replaceTMVar mv x = liftBase . atomically $ do
  void $ tryTakeTMVar mv
  putTMVar mv x
