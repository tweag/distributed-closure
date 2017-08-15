{-# LANGUAGE StaticPointers #-}

module Data.Profunctor.Static (StaticProfunctor(..)) where

import Control.Distributed.Closure
import Data.Typeable (Typeable)

-- | Instances of 'StaticProfunctor' should satisfy the following laws:
--
-- @
-- 'staticDimap' (static id) (static id) = static id
-- 'staticLmap' (static id) = static id
-- 'staticRmap' (static id) = static id
-- 'staticDimap' f g = staticLmap f . staticRmap g
-- @
class Typeable p => StaticProfunctor p where
  staticDimap
    :: (Typeable a, Typeable b, Typeable c, Typeable d)
    => Closure (a -> b) -> Closure (c -> d) -> p b c -> p a d
  staticDimap sf sg = staticLmap sf . staticRmap sg

  staticLmap
    :: (Typeable a, Typeable b, Typeable c)
    => Closure (a -> b) -> p b c -> p a c
  staticLmap sf = staticDimap sf (static id)

  staticRmap
    :: (Typeable a, Typeable c, Typeable d)
    => Closure (c -> d) -> p a c -> p a d
  staticRmap sg = staticDimap (static id) sg

  {-# MINIMAL staticDimap | staticLmap, staticRmap #-}

staticCompose
  :: (Typeable a, Typeable b, Typeable c)
  => Closure (b -> c) -> Closure (a -> b) -> Closure (a -> c)
staticCompose f g = static (.) `cap` f `cap` g

instance StaticProfunctor WrappedArrowClosure where
  staticDimap sf sg (WrapArrowClosure sk) =
    WrapArrowClosure (sg `staticCompose` sk `staticCompose` sf)
