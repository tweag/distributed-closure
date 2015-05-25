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
