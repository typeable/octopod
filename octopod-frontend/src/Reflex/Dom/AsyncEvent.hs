module Reflex.Dom.AsyncEvent
  ( NFData,
    asyncEventLast,
  )
where

import Control.Concurrent
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad.IO.Class
import Data.Functor
import Data.IORef
import Reflex

asyncEventLast ::
  (TriggerEvent t m, PerformEvent t m, MonadIO (Performable m), MonadIO m, NFData b) =>
  Event t a ->
  (a -> b) ->
  m (Event t b)
asyncEventLast aEv f = do
  lastThreadIdRef <- liftIO $ newIORef Nothing
  performEventAsync $
    aEv <&> \a fire -> liftIO $ do
      readIORef lastThreadIdRef >>= \case
        Nothing -> pure ()
        Just lastThreadId -> killThread lastThreadId
      newThreadId <- forkIO $ do
        b <- evaluate $ force $ f a
        fire b
      writeIORef lastThreadIdRef (Just newThreadId)
