{-# LANGUAGE StaticPointers #-}

module Data.Profunctor.Strong.Static where

import Control.Distributed.Closure
import Data.Profunctor.Static
import Data.Typeable (Typeable)

class StaticProfunctor p => StaticStrong p where
  staticFirst'
    :: (Typeable a, Typeable b, Typeable c)
    => p a b -> p (a, c) (b, c)
  staticSecond'
    :: (Typeable a, Typeable b, Typeable c)
    => p a b -> p (c, a) (c, b)

instance StaticStrong WrappedArrowClosure where
  staticFirst' (WrapArrowClosure sf) =
    WrapArrowClosure $ static (\f (x, y) -> (f x, y)) `cap` sf
  staticSecond' (WrapArrowClosure sf) =
    WrapArrowClosure $ static (\f (x, y) -> (x, f y)) `cap` sf
