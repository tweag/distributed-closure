{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Serializable closures for distributed programming.

{-# OPTIONS_GHC -funbox-strict-fields #-}

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
    -- $serializable-dicts
  , Dict(..)
  , ClosureDict(..)
  ) where

import           Control.Distributed.Closure.TH
import           Control.Distributed.Closure.Internal
import           Data.Constraint (Dict(..))

-- $serializable-dicts
--
-- A 'Dict' reifies a constraint in the form of a first class value. The 'Dict'
-- type is not serializable: how do you serialize the constraint that values of
-- this type carry? However, for any constraint @c@, a value of type @'Closure'
-- ('Dict' c)@ /can/ be serialized and sent over the wire, just like any
-- 'Closure'. A /serializable dictionary/ for some constraint @c@ is a value of
-- type @'Closure' ('Dict' c)@.
