{-# LANGUAGE StaticPointers #-}

module Data.Profunctor.Choice.Static where

import Control.Distributed.Closure
import Data.Bifunctor (bimap)
import Data.Profunctor.Static
import Data.Typeable (Typeable)

class StaticProfunctor p => StaticChoice p where
  staticLeft'
    :: (Typeable a, Typeable b, Typeable c)
    => p a b -> p (Either a c) (Either b c)
  staticRight'
    :: (Typeable a, Typeable b, Typeable c)
    => p a b -> p (Either c a) (Either c b)

instance StaticChoice WrappedArrowClosure where
  staticLeft' (WrapArrowClosure sf) =
    WrapArrowClosure $ static (\f -> bimap f id) `cap` sf
  staticRight' (WrapArrowClosure sg) =
    WrapArrowClosure $ static (\g -> bimap id g) `cap` sg
