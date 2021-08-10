{-# OPTIONS_GHC -fno-warn-orphans #-}

module Orphans () where

import Common.Types
import Data.Maybe
import Rel8

parseTypeInformationFromMapping :: (Eq a, Eq b, DBType b, Show b, Show a) => [(a, b)] -> TypeInformation a
parseTypeInformationFromMapping m =
  parseTypeInformation
    (\v -> maybe (Left $ "unknown value: " <> show v) Right . flip lookup reversedM $ v)
    (\v -> fromMaybe (error $ "forgot case: " <> show v) . flip lookup m $ v)
    typeInformation
  where
    reversedM = (\(x, y) -> (y, x)) <$> m

deriving via JSONBEncoded (Overrides l) instance (DBType (Overrides l))

deriving newtype instance DBType DeploymentId
deriving newtype instance DBEq DeploymentId

deriving newtype instance DBType DeploymentName
deriving newtype instance DBEq DeploymentName

deriving newtype instance DBType DeploymentTag
deriving newtype instance DBEq DeploymentTag

deriving via ReadShow Action instance DBType Action

deriving newtype instance DBType ArchivedFlag

deriving newtype instance DBType Duration

deriving newtype instance DBType Timestamp

deriving newtype instance DBType ProjectName

deriving anyclass instance DBEq DeploymentStatus
instance DBType DeploymentStatus where
  typeInformation = parseTypeInformationFromMapping deploymentStatusText

deriving via JSONBEncoded DeploymentMetadata instance DBType DeploymentMetadata

deriving newtype instance DBType Stdout
deriving newtype instance DBType Stderr

deriving newtype instance DBType ActionId
deriving newtype instance DBEq ActionId
