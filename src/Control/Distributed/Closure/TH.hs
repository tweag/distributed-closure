{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Utility Template Haskell functions.

module Control.Distributed.Closure.TH where

import           Control.Monad (replicateM)
import           Control.Distributed.Closure.Internal
import           Data.Constraint (Dict(..))
import qualified Language.Haskell.TH as TH
import           Numeric.Natural

cdict :: TH.ExpQ
cdict = cdictFrom 0

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
