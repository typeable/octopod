{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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

data Routes :: * -> * where
  DashboardRoute :: Routes (Maybe DeploymentName)
deriving instance Show (Routes a)

concat <$> mapM deriveRouteComponent
  [ ''Routes
  ]

routeEncoder
  :: Encoder (Either Text) Identity (R Routes) PageName
routeEncoder = handleEncoder (const (DashboardRoute :/ Nothing)) $
  pathComponentEncoder $ \case
    DashboardRoute -> PathSegment "stagings"
        $ maybeEncoder (unitEncoder mempty)
        $ singlePathSegmentEncoder . unwrappedEncoder
