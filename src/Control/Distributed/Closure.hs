{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , Dict(..)
  , ClosureDict(..)
  ) where

import           Control.Distributed.Closure.TH
import           Control.Distributed.Closure.Internal
import           Data.Constraint (Dict(..))

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
