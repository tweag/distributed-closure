{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Control.Distributed.Closure
import Control.Distributed.Closure.TH
import Data.Binary
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

data T a = T a

-- Test that the result of this splice compiles.
withStatic [d|
  instance Show a => Show (T a) where show = undefined
  instance (Eq a, Show a) => Eq (T a) where (==) = undefined
  |]

instance Arbitrary (Closure Int) where
  arbitrary = cpure $cdict <$> elements [0..4]

instance Arbitrary (Closure (Int -> Int)) where
  arbitrary =
      elements [ closure (static id)
               , closure (static pred)
               , closure (static succ)
               ]

instance Show (Closure a) where
  show _ = "<closure>"

main :: IO ()
main = hspec $ do
    describe "unclosure" $ do
      prop "is inverse to cpure" $ \x y z ->
        (unclosure . cpure $cdict) x == (x :: Int) &&
        (unclosure . cpure $cdict) y == (y :: Bool) &&
        (unclosure . cpure $cdict) z == (z :: Maybe Int)
      it "is inverse to closure" $ do
        (unclosure . closure) (static id) 0 `shouldBe` (0 :: Int)

    describe "laws" $ do
      prop "identity" $ \(v :: Closure Int) ->
        unclosure (closure (static id) `cap` v) == id (unclosure v)
      prop "composition" $ \(u :: Closure (Int -> Int))
                            (v :: Closure (Int -> Int))
                            (w :: Closure Int) ->
        unclosure (closure (static (.)) `cap` u `cap` v `cap` w) ==
        unclosure (u `cap` (v `cap` w))
      prop "homomorphism" $ \(f :: Closure (Int -> Int)) x ->
        unclosure (f `cap` x) == (unclosure f) (unclosure x)

    describe "serialization" $ do
      prop "decode is left inverse to encode" $ \v ->
          unclosure ((decode . encode) v) == unclosure (v :: Closure Int)
