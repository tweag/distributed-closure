{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Distributed.Closure
import Control.Distributed.Closure.TH
import Data.Binary
import Data.Bool (bool)
import Data.Typeable
import GHC.StaticPtr
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as QC

data T a = T a
data T1 a b = T1 a b
data T2 = T2

type family F a

-- Test that the result of this splice compiles.
withStatic [d|
  instance Show a => Show (T a) where show = undefined
  instance (Eq a, Show a) => Eq (T a) where (==) = undefined
  instance Show (F a) => Show (T1 a b) where show = undefined
  instance Show T2 where show = undefined
  |]

-- * Basic generators (parameterized by size)

-- | Generates a basic closure using @cpure@
genPure :: forall a. (Static (Serializable a), QC.Arbitrary a) => Int -> QC.Gen (Closure a)
genPure i =
    cpure (closureDict :: Closure (Dict (Serializable a))) <$>
      QC.resize (max 0 (i-1)) QC.arbitrary

-- | Generates a basic closure using @closure@
genStatic :: QC.Arbitrary (StaticPtr a) => Int -> QC.Gen (Closure a)
-- static pointers are considered to contribute 0 to the size, hence ignore the
-- size parameter.
genStatic _i = closure <$> QC.arbitrary

-- | Reifies basic datatypes (they must be @Serializable@ types). Only two types
-- here because we already have to enumerate a lot of cases manually (see below).
data Type a where
  TInt :: Type Int
  TBool :: Type Bool

instance Static (Binary Int) where closureDict = static Dict
instance Static (Typeable Int) where closureDict = static Dict

instance Static (Binary Bool) where closureDict = static Dict
instance Static (Typeable Bool) where closureDict = static Dict

-- | Existentially quantified version of 'Type'. So that they can be generated.
data AType where AType :: Typeable a => Type a -> AType

instance QC.Arbitrary (AType) where
  arbitrary =
      QC.elements [ AType TInt, AType TBool ]

-- | Composed types. Very few choices because of the combinatorics.
data Sig a where
  Zero :: Type a -> Sig a
  One :: Type a -> Type b -> Sig (a->b)
  Two :: Type a -> Type b -> Type c -> Sig (a->b->c)

-- | Extend a type with an extra parameter. May fail since functions in 'Sig'
-- have a maximum of two arguments.
push :: Type a -> Sig b -> Maybe (Sig (a -> b))
push a (Zero b) = Just $ One a b
push a (One b c) = Just $ Two a b c
push _ (Two _ _ _) = Nothing

-- | Non-recursive generator of atomic values for each type.
genSimple :: Sig a -> Int -> QC.Gen (Closure a)
genSimple (Zero TInt) = genPure
genSimple (Zero TBool) = genPure
genSimple (One TInt TInt) = genStatic
genSimple (One TBool TInt) = genStatic
genSimple (One TInt TBool) = genStatic `gap` genPure @Int
genSimple (One TBool TBool) = genStatic
genSimple (Two TInt TInt TInt) = genStatic
genSimple (Two TBool TInt TInt) = gflip genStatic
genSimple (Two TInt TBool TInt) = genStatic
genSimple (Two TBool TBool TInt) = genStatic
genSimple (Two TInt TInt TBool) = genStatic
genSimple (Two TBool TInt TBool) = genStatic
genSimple (Two TInt TBool TBool) = gflip genStatic
genSimple (Two TBool TBool TBool) = genStatic

gflip
  :: (Typeable a, Typeable b, Typeable c)
  => (Int -> QC.Gen (Closure (a->b->c))) -> Int -> QC.Gen (Closure (b->a->c))
gflip g i = (cap (static flip)) <$> g i

gap
  :: Typeable a
  => (Int -> QC.Gen (Closure (a->b)))
  -> (Int -> QC.Gen (Closure a))
  -> Int -> QC.Gen (Closure b)
gap gf gx i = do
  f <- gf i
  x <- gx i
  return $ f `cap` x

-- | Generate closures of a given type by randomly choosing to make the closure
-- a 'cap'. Stays within the boundaries of 'Sig' so that the type of the
-- function is also 'QC.Arbitrary'.
genClosure :: Sig a -> Int -> QC.Gen (Closure a)
genClosure sig size | size < 10 =
    genSimple sig size
genClosure sig size = do
    stop <- QC.frequency [(2, return True), (1, return False)]
    if stop then
      genSimple sig size
    else do
      let upper = div size 3
          lower = max 0 (size - 1 - upper)
      AType pivot <- QC.arbitrary
      case push pivot sig of
        Nothing -> genSimple sig size
        Just sig' -> do
          -- if the @size@ is big enough, then 1/3 of the time, if we can extend
          -- the signature, build a closure with @cap@
          function <- genClosure sig' upper
          argument <- genClosure (Zero pivot) lower
          return $ function `cap` argument

-- * Generating static pointers
--
-- Must be from explicit lists since static pointers are, well, static. The
-- combinatorics is unpleasant.

instance QC.Arbitrary (StaticPtr (Int -> Int)) where
  arbitrary =
      QC.elements
        [ static id
        , static pred
        , static succ
        , static (3*)
        ]

instance QC.Arbitrary (StaticPtr (Bool -> Int)) where
  arbitrary =
      QC.elements
       [ static (bool 0 1)
       , static (bool 57 42)]

instance QC.Arbitrary (StaticPtr (Bool -> Bool)) where
  arbitrary =
    QC.elements
      [ static id
      , static not ]

instance QC.Arbitrary (StaticPtr (Int -> Int -> Int)) where
  arbitrary =
      QC.elements
        [ static const
        , static (+)
        , static (*)
        , static (-)
        , static (\x y -> 2*x + y)
        ]

instance QC.Arbitrary (StaticPtr (Int -> Bool -> Int)) where
  arbitrary =
    QC.elements
      [ static const
      , static (\n b -> if b then n else -n)
      , static (bool 0)
      ]

instance QC.Arbitrary (StaticPtr (Bool -> Bool -> Int)) where
  arbitrary =
    QC.elements
      [ static (\x y -> bool 0 1 (x&&y))
      , static (\x y -> bool 57 42 (x||y))
      ]

instance QC.Arbitrary (StaticPtr (Int -> Int -> Bool)) where
  arbitrary =
    QC.elements
      [ static (==)
      , static (>=)
      , static (<=)
      , static (<)
      , static (>)
      ]

instance QC.Arbitrary (StaticPtr (Bool -> Int -> Bool)) where
  arbitrary =
    QC.elements
      [ static const
      , static (\b n -> b && (n >= 0))
      , static (\b n -> b || (n < 0))
      , static (\b n -> if b then n >=0 else n < 0)
      ]

instance QC.Arbitrary (StaticPtr (Bool -> Bool -> Bool)) where
  arbitrary =
    QC.elements
      [ static (&&)
      , static (||)]

-- * Instances

instance QC.Arbitrary (Closure Int) where
  arbitrary = QC.sized $ genClosure (Zero TInt)

instance QC.Arbitrary (Closure (Int -> Int)) where
  arbitrary = QC.sized $ genClosure (One TInt TInt)

instance Show (Closure a) where
  show _ = "<closure>"

instance Show (StaticPtr a) where
  show _ = "<static>"

-- | Extensional equality on closures (/i.e./ closures are equal if they
-- represent equal values)
instance Eq a => Eq (Closure a) where
  cl1 == cl2 =
    unclosure cl1 == unclosure cl2

-- * Tests

main :: IO ()
main = hspec $ do
    describe "unclosure" $ do
      prop "is inverse to cpure" $ \x y z ->
        (unclosure . cpure $cdict) x == (x :: Int) &&
        (unclosure . cpure $cdict) y == (y :: Bool) &&
        (unclosure . cpure $cdict) z == (z :: Maybe Int)
      prop "is inverse to cduplicate" $ \x ->
        (unclosure . cduplicate) x == (x :: Closure Int)
      prop "is inverse to closure of id" $ \(x :: Int) ->
        (unclosure . closure) (static id) x == x
      prop "is inverse to closure" $ \(f :: StaticPtr (Int -> Int))
                                      (x :: Int) ->
        (unclosure . closure) f x == deRefStaticPtr f x
    describe "laws" $ do
      prop "identity" $ \(v :: Closure Int) ->
        unclosure (static id `cap` v) == id (unclosure v)
      prop "composition" $ \(u :: Closure (Int -> Int))
                            (v :: Closure (Int -> Int))
                            (w :: Closure Int) ->
        closure (static (.)) `cap` u `cap` v `cap` w ==
        u `cap` (v `cap` w)
      prop "homomorphism" $ \(f :: Closure (Int -> Int)) x ->
        unclosure (f `cap` x) == (unclosure f) (unclosure x)

    describe "serialization" $ do
      prop "decode is left inverse to encode" $ \v ->
          (decode . encode) v == (v :: Closure Int)
