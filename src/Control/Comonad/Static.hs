{-# LANGUAGE StaticPointers #-}

module Control.Comonad.Static where

import Control.Distributed.Closure
import Data.Functor.Static
import Data.Typeable (Typeable)

-- | Instances of 'StaticExtend' should satisfy the following laws:
--
-- @
-- 'staticExtend' f = 'staticMap' f . 'staticDuplicate'
-- 'staticDuplicate' = 'staticExtend' (static 'id')
-- @
class StaticFunctor w => StaticExtend w where
  staticDuplicate :: Typeable a => w a -> w (w a)
  staticDuplicate = staticExtend (static id)

  staticExtend :: (Typeable a, Typeable b) => Closure (w a -> b) -> w a -> w b
  staticExtend sf = staticMap sf . staticDuplicate

  {-# MINIMAL staticDuplicate | staticExtend #-}

class StaticExtend w => StaticComonad w where
  staticExtract :: Typeable a => w a -> a

instance StaticExtend Closure where
  staticDuplicate = cduplicate

instance StaticComonad Closure where
  staticExtract = unclosure
