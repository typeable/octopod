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
import Data.IORef.Lifted
import Data.Fixed
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Time
import System.Mem.Weak

data CacheMap e c k v = CacheMap !(InternalCacheMap e k v) (forall m. c m => k -> m v) !NominalDiffTime

type InternalCacheMap e k v = IORef (Map k (UTCTime, MVar (Either e v)))

initCacheMap :: forall c e k v m.
  MonadBase IO m =>
  -- | Time after which a value is considered invalid. Not exact.
  NominalDiffTime ->
  -- | Time after which the value should be updated.
  -- (A value can still be valid and need updating – the caller will get the
  -- old valid value in that case.)
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
  -- Return the var with an error or result, or Nothing if the result needs to be computed and written to 'blank'
  mVar <- atomicModifyIORef' ref $ \m -> case M.lookup k m of
    Nothing -> (M.insert k (now, blank) m, Nothing)
    Just (timeCreated, var) -> if timeCreated < addUTCTime (negate update) now
      then (M.insert k (now, blank) m, Nothing)
      else (m, Just var)
  -- Return cached result (if no error), or a var where the result needs to be computed and written to
  result <- case mVar of
    Nothing -> pure $ Right blank
    Just var -> takeMVar var >>= \case
      Left _ -> pure $ Right var
      Right res -> pure $ Left res
  case result of
    Left res -> pure res
    Right var -> do
      v <- (Right <$> createValue k) `catchError` (pure . Left)
      putMVar var v
      liftEither v
