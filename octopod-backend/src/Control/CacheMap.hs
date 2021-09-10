module Control.CacheMap
  ( CacheMap,
    initCacheMap,
    lookupBlocking,
  )
where

import Control.Concurrent.Lifted
import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.Fixed
import Data.IORef.Lifted
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time
import System.Mem.Weak

data CacheMap m k v = CacheMap !(InternalCacheMap k v) !(k -> m v) !NominalDiffTime

type InternalCacheMap k v = IORef (Map k (UTCTime, MVar v))

initCacheMap ::
  -- | Time after which a value is considered invalid. Not exact.
  NominalDiffTime ->
  -- | Time after which the value should be updated.
  -- (A value can still be valid and need updating – the caller will get the
  -- old valid value in that case.)
  NominalDiffTime ->
  (k -> m v) ->
  IO (CacheMap m k v)
initCacheMap invalid update createValue = do
  ref <- newIORef M.empty
  wRef <- mkWeakIORef ref (pure ())
  void $ fork $ removeInvalidCaches invalid wRef
  pure $ CacheMap ref createValue update

removeInvalidCaches ::
  -- | Time after which a value is considered invalid. Not exact.
  NominalDiffTime ->
  Weak (InternalCacheMap k v) ->
  IO ()
removeInvalidCaches invalid wRef = do
  threadDelayMicro (realToFrac $ invalid / 10)
  cutoff <- addUTCTime (negate invalid) <$> getCurrentTime
  deRefWeak wRef >>= \case
    Nothing -> pure ()
    Just ref -> do
      atomicModifyIORef' ref $ \m -> (M.filter (\(t, _) -> t > cutoff) m, ())
      removeInvalidCaches invalid wRef
  where
    threadDelayMicro :: Micro -> IO ()
    threadDelayMicro (MkFixed i) = threadDelay (fromInteger i)

lookupBlocking :: Ord k => MonadBaseControl IO m => CacheMap m k v -> k -> m v
lookupBlocking (CacheMap mRef createValue update) k = do
  now <- liftBase getCurrentTime
  blank <- newEmptyMVar
  (vM, recreate) <-
    atomicModifyIORef'
      mRef
      ( \m -> case M.lookup k m of
          Nothing -> (M.insert k (now, blank) m, (blank, True))
          Just (timeCreated, vM)
            | timeCreated < addUTCTime (negate update) now ->
              (M.insert k (now, vM) m, (vM, True))
          Just (_, vM) -> (m, (vM, False))
      )

  when recreate $
    void $
      fork $ do
        v' <- createValue k
        replaceMVar vM v'

  readMVar vM

replaceMVar :: MonadBase IO m => MVar a -> a -> m ()
replaceMVar mv x = do
  void $ tryTakeMVar mv
  putMVar mv x
