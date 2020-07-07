module Common.Utils where

import Control.Lens
import Data.Monoid
import Data.Generics.Product

import Common.Types

-- Lens convenience helpers
(<^.>) :: Functor f => f a -> Getting b a b -> f b
(<^.>) fa l = fa <&> (^. l)

(<^?>) :: Functor f => f a -> Getting (First b) a b -> f (Maybe b)
(<^?>) fa l = fa <&> (^? l)

(<^..>) :: Functor f => f a -> Getting (Endo [b]) a b -> f [b]
(<^..>) fa t = fa <&> (^.. t)

infixl 8 <^.>, <^..>, <^?>

dfiName :: Getter DeploymentFullInfo DeploymentName
dfiName = field @"deployment" . field @"name"

isPending :: DeploymentStatus -> Bool
isPending = \case
  Running -> False
  Failure -> False
  CreatePending -> True
  UpdatePending -> True
  DeletePending -> True
