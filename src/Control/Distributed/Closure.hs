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

deriveClosureDict [t| Serializable () |]
deriveClosureDict
  [t| forall a b. (Serializable a, Serializable b) => Serializable (a, b) |]
deriveClosureDict
  [t| forall a b c. (Serializable a, Serializable b, Serializable c) => Serializable (a, b, c) |]
deriveClosureDict
  [t| forall a b c d. (Serializable a, Serializable b, Serializable c, Serializable d) => Serializable (a, b, c, d) |]
deriveClosureDict
  [t| forall a b c d e. (Serializable a, Serializable b, Serializable c, Serializable d, Serializable e) => Serializable (a, b, c, d, e) |]
deriveClosureDict
  [t| forall a b c d e f. (Serializable a, Serializable b, Serializable c, Serializable d, Serializable e, Serializable f) => Serializable (a, b, c, d, e, f) |]
deriveClosureDict
  [t| forall a b c d e f g. (Serializable a, Serializable b, Serializable c, Serializable d, Serializable e, Serializable f, Serializable g) => Serializable (a, b, c, d, e, f, g) |]
