{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
--Module      : Frontend.Route
--Description : Router setup.
--
--This module contains the type-level routing scheme and the router encoder.
--Also defines a 'Wrapped' instance for 'DeploymentName'. This helps
--simplify the router encoder.
--Routing is provided by the 'obelisk-router' package.
module Frontend.Route
  ( routeEncoder,
    Routes (..),
  )
where

import Control.Category ((.))
import Control.Lens (makeWrapped)
import Data.Functor.Identity
import Data.Text (Text)
import Obelisk.Route
import Obelisk.Route.TH
import Prelude hiding ((.))

import Common.Types
import Data.Kind

-- | Routing scheme definition.
data Routes :: Type -> Type where
  DashboardRoute :: Routes (Maybe DeploymentName)

deriving stock instance Show (Routes a)

fmap mconcat . sequence $
  [ makeWrapped ''DeploymentName'
  , deriveRouteComponent ''Routes
  ]

-- | URL encoder and decoder for 'Routes' scheme.
routeEncoder ::
  Encoder (Either Text) Identity (R Routes) PageName
routeEncoder = handleEncoder (const (DashboardRoute :/ Nothing)) $
  pathComponentEncoder $ \case
    DashboardRoute ->
      PathSegment "deployments" $
        maybeEncoder (unitEncoder mempty) $
          singlePathSegmentEncoder . unwrappedEncoder
