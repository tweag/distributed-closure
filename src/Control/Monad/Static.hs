{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Static where

import Control.Applicative.Static
import Control.Distributed.Closure
import Data.Functor.Static
import Data.Typeable (Typeable)

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

staticReturn :: (StaticApplicative m, Typeable a) => Closure a -> m a
staticReturn = staticPure

instance StaticBind Closure where
  staticBind m k = unclosure k (unclosure m)
