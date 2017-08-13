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

-- | Instances of 'StaticFunctor' should satisfy the following laws:
--
-- @
-- 'staticMap' (static 'id') = 'id'
-- 'staticMap' (static (.) ``cap`` f ``cap`` g) = 'staticMap' f . 'staticMap' g
-- @
class Typeable f => StaticFunctor f where
  staticMap :: (Typeable a, Typeable b) => Closure (a -> b) -> f a -> f b

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

-- | Instances of 'StaticBind' should satisfy the following laws (writing
-- 'staticMap', 'staticApply', 'staticBind' as infix @('<$>')@, @('<*>')@, @(>>=)@,
-- respectively):
--
-- @
-- (m >>= f) >>= g = m >>= static (.) ``cap`` (staticFlippedBind g) `cap` f
-- 'staticJoin' . 'staticJoin' = 'staticJoin' . 'staticMap' (static 'staticJoin')
-- @
--
-- where
--
-- @
-- staticFlippedBind :: Closure (b -> m c) -> Closure (m b -> m c)
-- staticFlippedBind = capDup (static (flip staticBind))
-- @
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

-- | Instances of 'StaticBifunctor' should satisfy the following laws:
--
-- @
-- 'staticBimap' (static id) (static id) = static id
-- 'staticFirst' (static id) = static id
-- 'staticSecond' (static id) = static id
-- 'staticBimap' f g = staticFirst f . staticSecond g
-- @
class Typeable p => StaticBifunctor p where
  staticBimap
    :: (Typeable a, Typeable b, Typeable c, Typeable d)
    => Closure (a -> b) -> Closure (c -> d) -> p a c -> p b d
  staticBimap sf sg = staticFirst sf . staticSecond sg

  staticFirst
    :: (Typeable a, Typeable b, Typeable c)
    => Closure (a -> b) -> p a c -> p b c
  staticFirst sf = staticBimap sf (static id)

  staticSecond
    :: (Typeable a, Typeable c, Typeable d)
    => Closure (c -> d) -> p a c -> p a d
  staticSecond sg = staticBimap (static id) sg

  {-# MINIMAL staticBimap | staticFirst, staticSecond #-}

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

class StaticProfunctor p => Strong p where
  staticFirst' :: p a b -> p (a, c) (b, c)
  staticSecond' :: p a b -> p (c, a) (c, b)

class StaticProfunctor p => Choice p where
  staticLeft' :: p a b -> p (Either a c) (Either b c)
  staticRight' :: p a b -> p (Either c a) (Either c b)

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
