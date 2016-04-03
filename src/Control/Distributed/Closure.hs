-- | Serializable closures for distributed programming. This package builds
-- a "remotable closure" abstraction on top of
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/static-pointers.html static pointers>.
-- See
-- <https://ocharles.org.uk/blog/guest-posts/2014-12-23-static-pointers.html this blog post>
-- for a longer introduction.

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
  ) where

import Control.Distributed.Closure.Internal
import Data.Constraint (Dict(..))

-- $serializable-dicts
--
-- A 'Dict' reifies a constraint in the form of a first class value. The 'Dict'
-- type is not serializable: how do you serialize the constraint that values of
-- this type carry? Whereas, for any constraint @c@, a value of type @'Closure'
-- ('Dict' c)@ /can/ be serialized and sent over the wire, just like any
-- 'Closure'. A /serializable dictionary/ for some constraint @c@ is a value of
-- type @'Closure' ('Dict' c)@.
