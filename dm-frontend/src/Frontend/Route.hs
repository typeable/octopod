{-|
Module      : Frontend.Route
Description : Router setup.

This module contains a type-level routing scheme and a router encoder. Also
there is an instance of 'Wrapped' definition for 'DeploymentName'. This helps to
simplify router encoder. Routing is provided by package @obelisk-router@.
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

-- | Routing scheme definition.
data Routes :: * -> * where
  DashboardRoute :: Routes (Maybe DeploymentName)
deriving instance Show (Routes a)

concat <$> mapM deriveRouteComponent
  [ ''Routes
  ]

-- | URL encoder and decoder.
routeEncoder
  :: Encoder (Either Text) Identity (R Routes) PageName
routeEncoder = handleEncoder (const (DashboardRoute :/ Nothing)) $
  pathComponentEncoder $ \case
    DashboardRoute -> PathSegment "stagings"
        $ maybeEncoder (unitEncoder mempty)
        $ singlePathSegmentEncoder . unwrappedEncoder
