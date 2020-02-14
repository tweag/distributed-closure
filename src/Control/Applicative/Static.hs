module Control.Applicative.Static where

import Data.Functor.Static
import Control.Distributed.Closure
import Data.Typeable (Typeable)

-- | Instances of 'StaticApply' should satisfy the following laws (writing
-- 'staticMap', 'staticApply' as infix @('<$>')@, @('<*>')@, respectively):
--
-- @
-- static (.) '<$>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)
-- x '<*>' (f '<$>' y) = (static (flip (.)) ``cap`` f) '<$>' x '<*>' y
-- f '<$>' (x '<*>' y) = (static (.) ``cap`` f) '<$>' x '<*>' y
-- @
class StaticFunctor f => StaticApply f where
  staticApply
    :: (Typeable a, Typeable b)
    => f (a -> b)
    -> f a
    -> f b

class StaticApply f => StaticApplicative f where
  staticPure :: Typeable a => Closure a -> f a

instance StaticApply Closure where
  staticApply = cap

instance StaticApplicative Closure where
  staticPure = id
