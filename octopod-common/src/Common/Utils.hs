{-|
Module      : Common.Utils
Description : Common utils for backend and frontend.

This module contains common utils between the backend and the frontend.
-}


module Common.Utils where

import           Control.Lens
import           Data.Generics.Product
import           Data.Monoid

import           Common.Types

-- Lens convenience helpers
(<^.>) :: Functor f => f a -> Getting b a b -> f b
(<^.>) fa l = fa <&> (^. l)

(<^?>) :: Functor f => f a -> Getting (First b) a b -> f (Maybe b)
(<^?>) fa l = fa <&> (^? l)

(<^..>) :: Functor f => f a -> Getting (Endo [b]) a b -> f [b]
(<^..>) fa t = fa <&> (^.. t)

infixl 8 <^.>, <^..>, <^?>

-- | Gets name from deployment full info.
dfiName :: Getter DeploymentFullInfo DeploymentName
dfiName = field @"deployment" . field @"name"

-- | Checks that deployment status is pending.
isPending :: DeploymentStatus -> Bool
isPending = \case
  Running -> False
  Failure _ -> False
  Archived -> False
  CreatePending -> True
  UpdatePending -> True
  ArchivePending -> True
