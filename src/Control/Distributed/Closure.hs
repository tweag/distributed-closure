-- | Serializable closures for distributed programming.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Distributed.Closure
  ( Serializable
    -- * Closures
  , Closure
  , closure
  , unclosure
  , cpure
  , cap
  , cmap
    -- * Closure dictionaries
    -- $static-dicts
  , Dict(..)
  , Static(..)
  ) where

import Control.Distributed.Closure.Internal
import Data.Constraint (Dict(..))
import Data.Typeable (Typeable)
import Unsafe.Coerce (unsafeCoerce)

-- $static-dicts
--
-- A 'Dict' reifies a constraint in the form of a first class value. The 'Dict'
-- type is not serializable: how do you serialize the constraint that values of
-- this type carry? Whereas, for any constraint @c@, a value of type @'Closure'
-- ('Dict' c)@ /can/ be serialized and sent over the wire, just like any
-- 'Closure'. A /static dictionary/ for some constraint @c@ is a value of type
-- @'Closure' ('Dict' c)@.

-- | It's often useful to create a static dictionary on-the-fly given any
-- constraint. Morally, all type class constraints have associated static
-- dictionaries, since these are either global values or simple combinations
-- thereof. But GHC doesn't yet know how to invent a static dictionary on-demand
-- yet given any type class constraint, so we'll have to do it manually for the
-- time being. By defining instances of this type class manually, or via
-- 'Control.Distributed.Closure.TH.withStatic' if it becomes too tedious.
class c => Static c where
  closureDict :: Closure (Dict c)

instance (a ~ b, Typeable a, Typeable b) => Static (a ~ b) where
  closureDict = unsafeCoerce $ closure (static (Dict :: Dict ()))
