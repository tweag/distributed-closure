{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Utility Template Haskell macros.

module Control.Distributed.Closure.TH where

import           Control.Monad (replicateM)
import           Control.Distributed.Closure.Internal
import           Data.Constraint (Dict(..))
import qualified Language.Haskell.TH as TH
import           Numeric.Natural

-- | @$(cstatic 'foo)@ is an abbreviation for @closure (static foo)@.
cstatic :: TH.Name -> TH.ExpQ
cstatic f = [| closure (static $(TH.varE f)) |]

-- | @$(cstaticDict 'foo)@ is an abbreviation for @closure (static foo) `cap`
-- $cdict@, a common pattern for implicitly feeding the static dictionary when
-- which dictionary to choose is clear from context.
cstaticDict :: TH.Name -> TH.ExpQ
cstaticDict f = [| closure (static $(TH.varE f)) `cap` $cdict |]

-- | Abbreviation for @closure (static Dict)@. Example usage:
--
-- @
-- foo :: Closure (Dict (Num a)) -> ...
--
-- foo $cdict ...
-- @
cdict :: TH.ExpQ
cdict = cdictFrom 0

-- | Create a static dictionary from the given dictionaries. Example usage:
--
-- @
-- $cdictFrom 2 $cdict $cdict :: Closure (Static (Dict (Eq a, Show a)))
-- @
cdictFrom :: Natural -> TH.ExpQ
cdictFrom n0 = apply abstract [| closure (static $(staticFun n0)) |] n0
  where
    staticFun 0 = [| Dict |]
    staticFun n = [| \Dict -> $(staticFun (n - 1)) |]
    apply k f n = do
        names <- replicateM (fromIntegral n) (TH.newName "x")
        k names (foldl (\acc x -> [| $acc `cap` $(TH.varE x) |]) f names)
    abstract [] expr = expr
    abstract (nm:names) expr = [| \ $(TH.varP nm) -> $(abstract names expr) |]
