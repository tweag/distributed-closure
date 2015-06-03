module Data.Appliance
  ( Appliance(..)
  ) where

-- | This class is equivalent to the Apply, but without
-- functor constraint. This class should be used that when
-- morally it's a function but it's not possible to express
-- 'fmap' due to limitations of the Haskell system, for example
-- if additional contrains or language extensions are needed
-- to lift computation to the context.
class Appliance k where
  (<.>) :: k (a -> b) -> k a -> k b
