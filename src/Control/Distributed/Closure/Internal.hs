{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}

-- | Private internals. You should not use this module unless you are determined
-- to monkey with the internals. This module comes with no API stability
-- guarantees whatsoever. Use at your own risks.

#if !MIN_VERSION_binary(0,7,6)
{-# OPTIONS_GHC -fno-warn-orphans #-} -- for binary < 0.7.6 compat.
#endif
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
#endif

module Control.Distributed.Closure.Internal
  ( Serializable
  , Closure(..)
  , closure
  , unclosure
  , cpure
  , cap
  , cmap
  , cduplicate
  ) where

import Data.Binary (Binary(..), Get, Put, decode, encode)
import Data.Binary.Put (putWord8)
import Data.Binary.Get (getWord8)
import Data.Constraint (Dict(..))
import Data.Typeable (Typeable)
import Data.ByteString.Lazy (ByteString)
import GHC.Base (Any)
#if !MIN_VERSION_binary(0,7,6)
import GHC.Fingerprint
#endif
import GHC.StaticPtr
import Unsafe.Coerce (unsafeCoerce) -- for dynClosureApply
import System.IO.Unsafe (unsafePerformIO)

-- | Values that can be sent across the network.
type Serializable a = (Binary a, Typeable a)

-- | Type of serializable closures. Abstractly speaking, a closure is a code
-- reference paired together with an environment. A serializable closure
-- includes a /shareable/ code reference (i.e. a 'StaticPtr'). Closures can be
-- serialized only if all expressions captured in the environment are
-- serializable.
data Closure a where
  -- XXX Can't unpack because of https://ghc.haskell.org/trac/ghc/ticket/12622.
  StaticPtr :: !(StaticPtr a) -> Closure a
  Encoded :: !ByteString -> Closure ByteString
  Ap :: !(Closure (a -> b)) -> !(Closure a) -> Closure b
  Duplicate :: Closure a -> Closure (Closure a)
  -- Cache the value a closure resolves to.
  Closure :: a -> !(Closure a) -> Closure a

#if MIN_VERSION_base(4,9,0)
instance IsStatic Closure where
  fromStaticPtr = closure
#endif

-- Will be obsoleted by https://ghc.haskell.org/trac/ghc/wiki/Typeable. We use
-- our own datatype instead of Dynamic in order to support dynClosureApply.
newtype DynClosure = DynClosure Any -- invariant: only values of type Closure.

-- | Until GHC.StaticPtr can give us a proper TypeRep upon decoding, we have to
-- pretend that this function doesn't need a 'Typeable' constraint to be safe.
toDynClosure :: Closure a -> DynClosure
toDynClosure = DynClosure . unsafeCoerce

fromDynClosure :: Typeable a => DynClosure -> Closure a
fromDynClosure (DynClosure x) = unsafeCoerce x

dynClosureApply :: DynClosure -> DynClosure -> DynClosure
dynClosureApply (DynClosure x1) (DynClosure x2) =
    case unsafeCoerce x1 of
      (clos1 :: Closure (a -> b)) -> case unsafeCoerce x2 of
        (clos2 :: Closure a) -> DynClosure $ unsafeCoerce $ Ap clos1 clos2

dynClosureDuplicate :: DynClosure -> DynClosure
dynClosureDuplicate (DynClosure x) =
    DynClosure $ unsafeCoerce $ Duplicate $ unsafeCoerce x

-- | Until GHC.StaticPtr can give us a proper TypeRep upon decoding, we have to
-- pretend that serializing/deserializing a @'Closure' a@ without a @'Typeable'
-- a@ constraint, i.e. for /any/ @a@, is safe.
putClosure :: Closure a -> Put
putClosure (StaticPtr sptr) = putWord8 0 >> put (staticKey sptr)
putClosure (Encoded bs) = putWord8 1 >> put bs
putClosure (Ap clos1 clos2) = putWord8 2 >> putClosure clos1 >> putClosure clos2
putClosure (Closure _ clos) = putClosure clos
putClosure (Duplicate clos) = putWord8 3 >> putClosure clos

getDynClosure :: Get DynClosure
getDynClosure = getWord8 >>= \case
    0 -> get >>= \key -> case unsafePerformIO (unsafeLookupStaticPtr key) of
           Just sptr -> return $ toDynClosure $ StaticPtr sptr
           Nothing -> fail $ "Static pointer lookup failed: " ++ show key
    1 -> toDynClosure . Encoded <$> get
    2 -> dynClosureApply <$> getDynClosure <*> getDynClosure
    3 -> dynClosureDuplicate <$> getDynClosure
    _ -> fail "Binary.get(Closure): unrecognized tag."

#if !MIN_VERSION_binary(0,7,6)
-- Orphan instance
instance Binary Fingerprint where
  put (Fingerprint x1 x2) = do
      put x1
      put x2
  get = do
      x1 <- get
      x2 <- get
      return $! Fingerprint x1 x2
#endif

instance Typeable a => Binary (Closure a) where
  put = putClosure
  get = do
      clos <- fromDynClosure <$> getDynClosure
      return $ Closure (unclosure clos) clos

-- | Lift a Static pointer to a closure with an empty environment.
closure :: StaticPtr a -> Closure a
closure sptr = Closure (deRefStaticPtr sptr) (StaticPtr sptr)

-- | Resolve a 'Closure' to the value that it represents. Calling 'unclosure'
-- multiple times on the same closure is efficient: for most argument values the
-- result is memoized.
unclosure :: Closure a -> a
unclosure (StaticPtr sptr) = deRefStaticPtr sptr
unclosure (Encoded x) = x
unclosure (Ap cf cx) = (unclosure cf) (unclosure cx)
unclosure (Closure x _) = x
unclosure (Duplicate x) = x

cduplicate :: Closure a -> Closure (Closure a)
cduplicate = Duplicate

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
cmap :: Typeable a => StaticPtr (a -> b) -> Closure a -> Closure b
cmap sf = cap (closure sf)
{-# DEPRECATED cmap "Use staticMap instead." #-}
