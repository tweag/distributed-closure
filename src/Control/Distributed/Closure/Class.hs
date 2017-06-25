-- | 'Closure' is not a functor, since we cannot map arbitrary functions over
-- it. But it sure looks like one, and an applicative one at that. What we can
-- do is map /static pointers to/ arbitrary functions over it (or in general,
-- closures). 'Closure' is not just an applicative functor, it's also a monad,
-- as well as a comonad, if again we limit the function space to those functions
-- that can be statically pointed to.
--
-- In fact an entire hierarchy of classes mirroring the standard classes can be
-- defined, where nearly the only difference lies in the fact that higher-order
-- arguments must be a proof of /static/-ness (i.e. a 'Closure'). The other
-- difference is that composing static values requires a proof of typeability,
-- so we carry those around ('Typeable' constraints).
--
-- This module defines just such a class hierarchy in the category of static
-- functions (aka values of type @'Closure' (a -> b)@).

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-} -- For StaticMonad instance.

module Control.Distributed.Closure.Class
  ( Static(..)
  , StaticFunctor(..)
  , StaticApply(..)
  , StaticBind(..)
  , StaticApplicative(..)
  , StaticMonad
  , staticReturn
  , StaticExtend(..)
  , StaticComonad(..)
  ) where

import Control.Distributed.Closure
import Data.Typeable (Typeable)

class Typeable f => StaticFunctor f where
  staticMap :: (Typeable a, Typeable b) => Closure (a -> b) -> f a -> f b

class StaticFunctor f => StaticApply f where
  staticApply
    :: (Typeable a, Typeable b)
    => f (Closure (a -> b))
    -> f a
    -> f b

class StaticApply f => StaticApplicative f where
  staticPure :: Typeable a => a -> f a

class StaticApply m => StaticBind m where
  staticBind :: (Typeable a, Typeable b) => m a -> Closure (a -> m b) -> m b
  staticBind m k = staticJoin (staticMap k m)

  staticJoin :: Typeable a => m (m a) -> m a
  staticJoin m = m `staticBind` static id

  {-# MINIMAL staticBind | staticJoin #-}

class (StaticApplicative m, StaticBind m) => StaticMonad m
instance (StaticApplicative m, StaticBind m) => StaticMonad m

staticReturn :: (StaticApplicative m, Typeable a) => a -> m a
staticReturn = staticPure

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

instance StaticFunctor Closure where
  staticMap = cap

instance StaticApply Closure where
  staticApply = cap . unclosure

instance StaticBind Closure where
  staticBind m k = unclosure k (unclosure m)

instance StaticExtend Closure where
  staticDuplicate = cduplicate

instance StaticComonad Closure where
  staticExtract = unclosure
