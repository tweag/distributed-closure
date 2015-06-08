{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}

-- | Private internals. You should not use this module unless you are determined
-- to monkey with the internals. This module comes with no API stability
-- guarantees whatsoever. Use at your own risks.

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Control.Distributed.Closure.Internal
  ( Serializable
  , Closure(..)
  , closure
  , unclosure
  , cpure
  , cap
  , cmap
  ) where

import           Data.Binary (Binary, decode, encode)
import Data.Constraint (Dict(..))
import Data.Typeable (Typeable)
import Data.ByteString.Lazy (ByteString)
import GHC.StaticPtr

-- | Values that can be sent across the network.
type Serializable a = (Binary a, Typeable a)

-- | Type of serializable closures. Abstractly speaking, a closure is a code
-- reference paired together with an environment. A serializable closure
-- includes a /shareable/ code reference (i.e. a 'StaticPtr'). Closures can be
-- serialized only if all expressions captured in the environment are
-- serializable.
data Closure a where
  StaticPtr :: !(StaticPtr b) -> Closure b
  Encoded :: !ByteString -> Closure ByteString
  Ap :: !(Closure (b -> c)) -> !(Closure b) -> Closure c
  Closure :: Closure a -> a -> Closure a

-- | Lift a Static pointer to a closure with an empty environment.
closure :: StaticPtr a -> Closure a
closure = StaticPtr

-- | Resolve a 'Closure' to the value that it represents.
unclosure :: Closure a -> a
unclosure (StaticPtr sptr) = deRefStaticPtr sptr
unclosure (Encoded x) = x
unclosure (Ap cf cx) = (unclosure cf) (unclosure cx)
unclosure (Closure cx x) = x

decodeD :: Dict (Serializable a) -> ByteString -> a
decodeD Dict = decode

-- | A closure can be created from any serializable value. 'cpure' corresponds
-- to "Control.Applicative"'s 'Control.Applicative.pure', but restricted to
-- lifting serializable values only.
cpure :: Closure (Dict (Serializable a)) -> a -> Closure a
cpure cdict x | Dict <- unclosure cdict =
    Closure x $
    StaticPtr (static decodeD) `cap`
    cdict `cap`
    Encoded (encode x)

-- | Closure application. Note that 'Closure' is not a functor, let alone an
-- applicative functor, even if it too has a meaningful notion of application.
cap :: Typeable a          -- XXX 'Typeable' constraint only for forward compat.
    => Closure (a -> b)
    -> Closure a
    -> Closure b
cap (Closure f closf) (Closure x closx) = Closure (f x) (Ap closf closx)
cap closf closx = Ap closf closx

-- | 'Closure' is not a 'Functor', in that we cannot map arbitrary functions
-- over it. That is, we cannot define 'fmap'. However, we can map a static
-- pointer to a function over a 'Closure'.
cmap :: StaticPtr (a -> b) -> Closure a -> Closure b
cmap = Ap . StaticPtr
