{-|
Module      : Frontend.Route
Description : Router setup.

This module contains the type-level routing scheme and the router encoder.
Also defines a 'Wrapped' instance for 'DeploymentName'. This helps
simplify the router encoder.
Routing is provided by the 'obelisk-router' package.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.Route where

import           Control.Category ((.))
import           Control.Lens (makeWrapped)
import           Data.Functor.Identity
import           Data.Text (Text)
import           Obelisk.Route
import           Obelisk.Route.TH
import           Prelude hiding ((.))

import           Common.Types

makeWrapped ''DeploymentName

-- | Routing scheme definition.
data Routes :: * -> * where
  DashboardRoute :: Routes (Maybe DeploymentName)
deriving instance Show (Routes a)

concat <$> mapM deriveRouteComponent
  [ ''Routes
  ]

-- | URL encoder and decoder for 'Routes' scheme.
routeEncoder
  :: Encoder (Either Text) Identity (R Routes) PageName
routeEncoder = handleEncoder (const (DashboardRoute :/ Nothing)) $
  pathComponentEncoder $ \case
    DashboardRoute -> PathSegment "deployments"
      $ maybeEncoder (unitEncoder mempty)
      $ singlePathSegmentEncoder . unwrappedEncoder
