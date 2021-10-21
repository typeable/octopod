module Control.Octopod.DeploymentLock
  ( LockedDeployments,
    initLockedDeployments,
    withLockedDeployment,
    isDeploymentLocked,
  )
where

import Control.Arrow
import Common.Types
import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Generics.Product.Typed
import Data.IORef.Lifted
import Data.Set (Set)
import qualified Data.Set as S

newtype LockedDeployments = LockedDeployments (IORef (Set DeploymentName))

initLockedDeployments :: MonadBase IO m => m LockedDeployments
initLockedDeployments = LockedDeployments <$> newIORef S.empty

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
withLockedDeployment dName conflict act = do
  LockedDeployments ref <- asks getTyped
  bracket
    (atomicModifyIORef' ref (S.insert dName &&& S.notMember dName))
    (\ok -> when ok $ atomicModifyIORef' ref (S.delete dName &&& const ()))
    (\ok -> if ok then act else conflict)

isDeploymentLocked ::
  (MonadReader r m, HasType LockedDeployments r, MonadBase IO m) =>
  DeploymentName ->
  m Bool
isDeploymentLocked dName = do
  LockedDeployments ref <- asks getTyped
  S.member dName <$> readIORef ref
