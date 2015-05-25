{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

module Control.Distributed.Closure.Internal where

import           Data.Binary (Binary, decode, encode)
import           Data.Constraint (Dict(..), (:-)(..), mapDict)
import           Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as BSL
import           Data.ByteString.Lazy (ByteString)
import           GHC.StaticPtr

class (Binary a, Typeable a) => Serializable a
instance (Binary a, Typeable a) => Serializable a

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
cpure :: ClosureDict (Serializable a) => a -> Closure a
cpure x =
    StaticPtr (static decodeD) `cap`
    closureDict `cap`
    Encoded (encode x)

-- | Closure application. Note that 'Closure' is not a functor, let alone an
-- applicative functor, even if it too has a meaningful notion of application.
cap :: Closure (a -> b) -> Closure a -> Closure b
cap = Ap

-- | Reify constraints as /static dictionaries/. That is, obtain
-- a representation as a first class value of the implicit dictionary
-- corresponding to type class constraints in function signatures. This value
-- can be passed around and serialized, just like any other 'Closure'.
class c => ClosureDict c where
  -- | A static dictionary corresponding to the instance.
  closureDict :: Closure (Dict c)

instance ClosureDict () where
  closureDict = closure (static Dict)

instance (ClosureDict a, Typeable a, ClosureDict b, Typeable b)
      => ClosureDict (a, b) where
  closureDict =
    closure (static (\Dict Dict -> Dict))
      `cap` (closureDict :: Closure (Dict a))
      `cap` (closureDict :: Closure (Dict b))

instance ( ClosureDict a, Typeable a
         , ClosureDict b, Typeable b
         , ClosureDict c, Typeable c
         ) => ClosureDict (a, b, c) where
  closureDict =
    closure (static (\Dict Dict Dict -> Dict))
      `cap` (closureDict :: Closure (Dict a))
      `cap` (closureDict :: Closure (Dict b))
      `cap` (closureDict :: Closure (Dict c))

instance ( ClosureDict a, Typeable a
         , ClosureDict b, Typeable b
         , ClosureDict c, Typeable c
         , ClosureDict d, Typeable d
         ) => ClosureDict (a, b, c, d) where
  closureDict =
    closure (static (\Dict Dict Dict Dict -> Dict))
      `cap` (closureDict :: Closure (Dict a))
      `cap` (closureDict :: Closure (Dict b))
      `cap` (closureDict :: Closure (Dict c))
      `cap` (closureDict :: Closure (Dict d))

instance ( ClosureDict a, Typeable a
         , ClosureDict b, Typeable b
         , ClosureDict c, Typeable c
         , ClosureDict d, Typeable d
         , ClosureDict e, Typeable e
         ) => ClosureDict (a, b, c, d, e) where
  closureDict =
    closure (static (\Dict Dict Dict Dict Dict -> Dict))
      `cap` (closureDict :: Closure (Dict a))
      `cap` (closureDict :: Closure (Dict b))
      `cap` (closureDict :: Closure (Dict c))
      `cap` (closureDict :: Closure (Dict d))
      `cap` (closureDict :: Closure (Dict e))

instance ( ClosureDict a, Typeable a
         , ClosureDict b, Typeable b
         , ClosureDict c, Typeable c
         , ClosureDict d, Typeable d
         , ClosureDict e, Typeable e
         , ClosureDict f, Typeable f
         ) => ClosureDict (a, b, c, d, e, f) where
  closureDict =
    closure (static (\Dict Dict Dict Dict Dict Dict -> Dict))
      `cap` (closureDict :: Closure (Dict a))
      `cap` (closureDict :: Closure (Dict b))
      `cap` (closureDict :: Closure (Dict c))
      `cap` (closureDict :: Closure (Dict d))
      `cap` (closureDict :: Closure (Dict e))
      `cap` (closureDict :: Closure (Dict f))

instance ( ClosureDict a, Typeable a
         , ClosureDict b, Typeable b
         , ClosureDict c, Typeable c
         , ClosureDict d, Typeable d
         , ClosureDict e, Typeable e
         , ClosureDict f, Typeable f
         , ClosureDict g, Typeable g
         ) => ClosureDict (a, b, c, d, e, f, g) where
  closureDict =
    closure (static (\Dict Dict Dict Dict Dict Dict Dict -> Dict))
      `cap` (closureDict :: Closure (Dict a))
      `cap` (closureDict :: Closure (Dict b))
      `cap` (closureDict :: Closure (Dict c))
      `cap` (closureDict :: Closure (Dict d))
      `cap` (closureDict :: Closure (Dict e))
      `cap` (closureDict :: Closure (Dict f))
      `cap` (closureDict :: Closure (Dict g))

instance ( ClosureDict a, Typeable a
         , ClosureDict b, Typeable b
         , ClosureDict c, Typeable c
         , ClosureDict d, Typeable d
         , ClosureDict e, Typeable e
         , ClosureDict f, Typeable f
         , ClosureDict g, Typeable g
         , ClosureDict h, Typeable h
         ) => ClosureDict (a, b, c, d, e, f, g, h) where
  closureDict =
    closure (static (\Dict Dict Dict Dict Dict Dict Dict Dict -> Dict))
      `cap` (closureDict :: Closure (Dict a))
      `cap` (closureDict :: Closure (Dict b))
      `cap` (closureDict :: Closure (Dict c))
      `cap` (closureDict :: Closure (Dict d))
      `cap` (closureDict :: Closure (Dict e))
      `cap` (closureDict :: Closure (Dict f))
      `cap` (closureDict :: Closure (Dict g))
      `cap` (closureDict :: Closure (Dict h))
