-- | This module reexports "GHC.StaticPtr" with a few extra utilities.

{-# LANGUAGE StaticPointers #-}

module Control.Distributed.StaticPtr
  ( module GHC.StaticPtr
    -- * Static class hierarchy
    -- $static-hier
  , StaticFunctor(..)
  , StaticApplicative(..)
  , StaticMonad(..)
  , staticReturn
  , StaticComonad(..)
  ) where

import Control.Distributed.Closure.Internal
import Data.Typeable (Typeable)
import GHC.StaticPtr

-- $static-hier
--
-- 'Closure' is not a functor, since we cannot map arbitrary functions over it.
-- But it sure looks like one, and an applicative one at that. What we can do is
-- map /static pointers to/ arbitrary functions over it. 'Closure' is not just
-- an applicative functor, it's also a monad, as well as a comonad, if again we
-- limit the function space to those functions that can be statically pointed
-- to.
--
-- In fact an entire hierarchy of classes mirroring the standard classes can be
-- defined, where the only difference lies in the fact that all pure values'
-- types appearing in negative position must be a proof of /static/-ness. That
-- is, a function cannot in general produce a closure if the only argument /is
-- not/ static (@a -> 'Closure' a@ won't work), but conversely a function cannot
-- produce anything that /is/ static given only a closure (@'Closure' a ->
-- StaticPtr a@ won't work).

class Typeable f => StaticFunctor f where
  staticMap :: (Typeable a, Typeable b) => StaticPtr (a -> b) -> f a -> f b

instance StaticFunctor Closure where
  staticMap sf = cap (closure sf)

class StaticFunctor f => StaticApplicative f where
  staticPure :: Typeable a => StaticPtr a -> f a
  staticAp :: (Typeable a, Typeable b) => f (a -> b) -> f a -> f b

instance StaticApplicative Closure where
  staticPure = closure
  staticAp = cap

class StaticApplicative m => StaticMonad m where
  staticBind :: (Typeable a, Typeable b) => m a -> StaticPtr (a -> m b) -> m b
  staticBind m k = staticJoin (staticMap k m)

  staticJoin :: Typeable a => m (m a) -> m a
  staticJoin m = m `staticBind` static id

  {-# MINIMAL staticBind | staticJoin #-}

staticReturn :: (StaticMonad m, Typeable a) => StaticPtr a -> m a
staticReturn = staticPure

instance StaticMonad Closure where
  staticBind m sk = deRefStaticPtr sk (unclosure m)

class StaticFunctor w => StaticComonad w where
  staticExtract :: w a -> a

  staticDuplicate :: Typeable a => w a -> w (w a)
  staticDuplicate = staticExtend (static id)

  staticExtend :: (Typeable a, Typeable b) => StaticPtr (w a -> b) -> w a -> w b
  staticExtend sf = staticMap sf . staticDuplicate

  {-# MINIMAL staticExtract, (staticDuplicate | staticExtend) #-}

instance StaticComonad Closure where
  staticExtract = unclosure
  staticDuplicate = cduplicate
