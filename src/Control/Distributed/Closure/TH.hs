{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Utility Template Haskell functions.

module Control.Distributed.Closure.TH where

import           Control.Monad (liftM)
import           Data.Constraint (Dict(..), (:-)(..), mapDict)
import           Control.Distributed.Closure.Internal
import           GHC.StaticPtr
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
--
-- This function *require* ScopedTypeVariable language extension.
deriveClosureDict :: TH.Q [TH.InstanceDec] -> TH.DecsQ
deriveClosureDict = (>>= liftM concat . mapM go)
  where
    go (TH.InstanceD cxt hd _) = do
        let constraintQ = return $ foldl TH.AppT (TH.TupleT (length cxt)) cxt
            hdQ = return hd
        [d| instance ClosureDict $constraintQ => ClosureDict $hdQ where
              closureDict =
                  closure (static (mapDict (Sub Dict)))
                    `cap` (closureDict :: Closure (Dict $constraintQ)) |]
