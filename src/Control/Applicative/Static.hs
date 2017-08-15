module Control.Applicative.Static where

import Data.Functor.Static
import Control.Distributed.Closure
import Data.Typeable (Typeable)

-- | Instances of 'StaticApply' should satisfy the following laws (writing
-- 'staticMap', 'staticApply' as infix @('<$>')@, @('<*>')@, respectively):
--
-- @
-- static staticCompose '<$>' u '<*>' v '<*>' w = u '<*>' (v '<*>' w)
-- x '<*>' (f '<$>' y) = (static (flip staticCompose) ``capDup`` f) '<$>' x '<*>' y
-- f '<$>' (x '<*>' y) = (static staticCompose ``capDup`` f) '<$>' x '<*>' y
-- @
--
-- where
--
-- @
-- staticCompose :: Closure (b -> c) -> Closure (a -> b) -> Closure (a -> c)
-- staticCompose f g = static (.) ``cap`` f ``cap`` g
-- @
class StaticFunctor f => StaticApply f where
  staticApply
    :: (Typeable a, Typeable b)
    => f (Closure (a -> b))
    -> f a
    -> f b

class StaticApply f => StaticApplicative f where
  staticPure :: Typeable a => a -> f a

instance StaticApply Closure where
  staticApply = cap . unclosure
