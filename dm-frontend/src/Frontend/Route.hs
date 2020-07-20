{-|
Module      : Frontend.Route
Description : Project router definition.

This module contains type-level routing scheme and encoder for route. Alse there
is a  'Wrapped' instance definition for 'DeploymentName'. This helps to simplify
router encoder. Routing is provided by package @obelisk-router@.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend.Route where

import Control.Category ((.))
import Control.Lens (makeWrapped)
import Data.Text (Text)
import Data.Functor.Identity
import Obelisk.Route
import Obelisk.Route.TH
import Prelude hiding ((.))

import Common.Types

makeWrapped ''DeploymentName

-- | Definition of router scheme:
-- @
-- stagings/\<deployment_name\>
-- @
data Routes :: * -> * where
  DashboardRoute :: Routes (Maybe DeploymentName)
deriving instance Show (Routes a)

concat <$> mapM deriveRouteComponent
  [ ''Routes
  ]

-- | Encoder and decoder for routes.
routeEncoder
  :: Encoder (Either Text) Identity (R Routes) PageName
routeEncoder = handleEncoder (const (DashboardRoute :/ Nothing)) $
  pathComponentEncoder $ \case
    DashboardRoute -> PathSegment "stagings"
        $ maybeEncoder (unitEncoder mempty)
        $ singlePathSegmentEncoder . unwrappedEncoder
