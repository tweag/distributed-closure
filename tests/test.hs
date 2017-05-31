{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Test.QuickCheck

data T a = T a

-- Test that the result of this splice compiles.
withStatic [d|
  instance Show a => Show (T a) where show = undefined
  instance (Eq a, Show a) => Eq (T a) where (==) = undefined
  |]

-- * Basic generators (parameterized by size)

-- | Generates a basic closure using @cpure@
genPure :: forall a. (Static (Serializable a), Arbitrary a) => Int -> Gen (Closure a)
genPure i =
    cpure (closureDict :: Closure (Dict (Serializable a))) <$>
      resize (max 0 (i-1)) arbitrary

-- | Generates a basic closure using @closure@
genStatic :: Arbitrary (StaticPtr a) => Int -> Gen (Closure a)
-- static pointers are considered to contribute 0 to the size, hence ignore the
-- size parameter.
genStatic _i = closure <$> arbitrary

-- | Reifies basic datatypes (they must be @Serializable@ types). Only two types
-- here because we already have to enumerate a lot of cases manually (see below).
data Type a where
  TInt :: Type Int
  TBool :: Type Bool

instance Static (Serializable Int) where
  closureDict = static Dict

instance Static (Serializable Bool) where
  closureDict = static Dict

-- | Existentially quantified version of 'Type'. So that they can be generated.
data AType where AType :: Typeable a => Type a -> AType

instance Arbitrary (AType) where
  arbitrary =
      elements [ AType TInt, AType TBool ]

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
genSimple :: Sig a -> Int -> Gen (Closure a)
genSimple (Zero TInt) = genPure
genSimple (Zero TBool) = genPure
genSimple (One TInt TInt) = genStatic
genSimple (One TBool TInt) = genStatic
genSimple (One TInt TBool) = genStatic
genSimple (One TBool TBool) = genStatic
genSimple (Two TInt TInt TInt) = genStatic
genSimple (Two TBool TInt TInt) = genStatic
genSimple (Two TInt TBool TInt) = genStatic
genSimple (Two TBool TBool TInt) = genStatic
genSimple (Two TInt TInt TBool) = genStatic
genSimple (Two TBool TInt TBool) = genStatic
genSimple (Two TInt TBool TBool) = genStatic
genSimple (Two TBool TBool TBool) = genStatic

-- | Generate closures of a given type by randomly choosing to make the closure
-- a 'cap'. Stays within the boundaries of 'Sig' so that the type of the
-- function is also 'Arbitrary'.
genClosure :: Sig a -> Int -> Gen (Closure a)
genClosure sig size | size < 10 =
    genSimple sig size
genClosure sig size = do
    stop <- frequency [(2, return True), (1, return False)]
    if stop then
      genSimple sig size
    else do
      let upper = div size 3
          lower = max 0 (size - 1 - upper)
      AType pivot <- arbitrary
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

instance Arbitrary (StaticPtr (Int -> Int)) where
  arbitrary =
      elements
        [ static id
        , static pred
        , static succ
        , static (3*)
        ]

instance Arbitrary (StaticPtr (Bool -> Int)) where
  arbitrary =
      elements
       [ static (bool 0 1)
       , static (bool 57 42)]

instance Arbitrary (StaticPtr (Int -> Bool)) where
  arbitrary =
      elements
        [ static (== 0)
        , static (<= 0)
        , static (> 0)
        ]

instance Arbitrary (StaticPtr (Bool -> Bool)) where
  arbitrary =
    elements
      [ static id
      , static not ]

instance Arbitrary (StaticPtr (Int -> Int -> Int)) where
  arbitrary =
      elements
        [ static const
        , static (+)
        , static (*)
        , static (-)
        , static (\x y -> 2*x + y)
        ]

instance Arbitrary (StaticPtr (Bool -> Int -> Int)) where
  arbitrary =
    elements
      [ static (\b n -> if b then n else -n)
      , static (flip const)
      ]

instance Arbitrary (StaticPtr (Int -> Bool -> Int)) where
  arbitrary =
    elements
      [ static const
      , static (bool 0)
      ]

instance Arbitrary (StaticPtr (Bool -> Bool -> Int)) where
  arbitrary =
    elements
      [ static (\x y -> bool 0 1 (x&&y))
      , static (\x y -> bool 57 42 (x||y))
      ]

instance Arbitrary (StaticPtr (Int -> Int -> Bool)) where
  arbitrary =
    elements
      [ static (==)
      , static (>=)
      , static (<)
      ]

instance Arbitrary (StaticPtr (Bool -> Int -> Bool)) where
  arbitrary =
    elements
      [ static const
      , static (\b n -> b && (n >= 0))
      , static (\b n -> b || (n < 0))
      ]

instance Arbitrary (StaticPtr (Int -> Bool -> Bool)) where
  arbitrary =
    elements
      [ static (flip const)
      , static (\n b -> if b then n >=0 else n < 0)]

instance Arbitrary (StaticPtr (Bool -> Bool -> Bool)) where
  arbitrary =
    elements
      [ static (&&)
      , static (||)]

-- * Instances

instance Arbitrary (Closure Int) where
  arbitrary = sized $ genClosure (Zero TInt)

instance Arbitrary (Closure (Int -> Int)) where
  arbitrary = sized $ genClosure (One TInt TInt)

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
