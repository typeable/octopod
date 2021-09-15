module Control.Octopod.DeploymentLock
  ( LockedDeployments,
    initLockedDeployments,
    withLockedDeployment,
    isDeploymentLocked,
  )
where

import Common.Types
import Control.Concurrent.STM
import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Generics.Product.Typed
import Data.Set (Set)
import qualified Data.Set as S

newtype LockedDeployments = LockedDeployments (TVar (Set DeploymentName))

initLockedDeployments :: IO LockedDeployments
initLockedDeployments = LockedDeployments <$> newTVarIO S.empty

withLockedDeployment ::
  ( MonadReader r m
  , HasType LockedDeployments r
  , MonadBaseControl IO m
  ) =>
  DeploymentName ->
  -- | The conflict handler. Gets called if the deployment is
  -- already being processed.
  m a ->
  -- | The actions to be performed when the deployment is locked.
  m a ->
  m a
withLockedDeployment dName conflictHandler m = do
  (LockedDeployments tvar) <- asks getTyped
  proceed <- liftBase . atomically $ do
    s <- readTVar tvar
    if S.member dName s
      then return False -- Do not proceed
      else do
        writeTVar tvar (S.insert dName s)
        return True -- Proceed
  if proceed
    then finally m $ liftBase . atomically $ modifyTVar tvar (S.delete dName)
    else conflictHandler

isDeploymentLocked ::
  (MonadReader r m, HasType LockedDeployments r, MonadBase IO m) =>
  DeploymentName ->
  m Bool
isDeploymentLocked dName = do
  (LockedDeployments tvar) <- asks getTyped
  s <- liftBase $ readTVarIO tvar
  return $ S.member dName s
