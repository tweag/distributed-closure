{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Utility Template Haskell functions.

module Control.Distributed.Closure.TH where

import           Control.Monad (liftM, replicateM)
import           Control.Distributed.Closure.Internal
import           Data.Constraint (Dict(..), (:-)(..), mapDict)
import           Data.List (nub)
import           Data.Typeable (Typeable)
import           GHC.StaticPtr
import           Data.Generics.Schemes (listify)
import qualified Language.Haskell.TH as TH

-- | Derive a 'ClosureDict' instance for the given instance context and instance
-- head. Examples:
--
-- > deriveClosureDict [d| instance Eq Int |]
-- > deriveClosureDict [d| instance Eq Bool |]
-- > deriveClosureDict [d| instance (Eq a, Eq b) => Eq (a, b) |]
-- > deriveClosureDict [d| instance Typeable a => Serializable (Proxy a) |]
--
-- which each derive the following instances,
--
-- > instance ClosureDict (Eq Int) where closureDict = closure (static Dict)
-- > instance ClosureDict (Eq Bool) where closureDict = closure (static Dict)
-- > instance (ClosureDict (Eq a), ClosureDict (Eq b)) => ClosureDict (Eq (a, b)) where ...
-- > instance ClosureDict (Typeable a) => ClosureDict (Serializable (Proxy a)) where ...
deriveClosureDict :: TH.Q [TH.InstanceDec] -> TH.DecsQ
deriveClosureDict = (>>= liftM concat . mapM go)
  where
    go (TH.InstanceD cxt hd _) = do
        let -- Typeable constraints for all type variables.
            typeableCtx =
                mapM (\var -> [t| Typeable $(return var) |]) $
                nub $
                listify (\case TH.VarT _ -> True; _ -> False) cxt ++
                listify (\case TH.VarT _ -> True; _ -> False) hd
        constraints <- (cxt ++) <$> typeableCtx
        let constraintQ =
              return $
              foldl TH.AppT (TH.TupleT (length constraints)) constraints
            hdQ = return hd
        [d| instance ClosureDict $constraintQ => ClosureDict $hdQ where
              closureDict =
                  closure (static (mapDict (Sub Dict)))
                    `cap` (closureDict :: Closure (Dict $constraintQ)) |]

forallInstances :: TH.Name -> (TH.Q [TH.InstanceDec] -> TH.DecsQ) -> TH.DecsQ
forallInstances cls f = do
    c <- TH.newName "c"
    f (TH.reifyInstances cls [TH.VarT c])
