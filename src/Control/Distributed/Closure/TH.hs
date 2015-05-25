{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Utility Template Haskell functions.

module Control.Distributed.Closure.TH where

import           Data.Constraint (Dict(..), (:-)(..), mapDict)
import           Control.Distributed.Closure.Internal
import           GHC.StaticPtr
import qualified Language.Haskell.TH as TH

deriveClosureDict :: TH.TypeQ -> TH.DecsQ
deriveClosureDict = (>>= go)
  where
    go (TH.ForallT _ cxt hd) = do
        let constraintQ = return $ foldl TH.AppT (TH.TupleT (length cxt)) cxt
            hdQ = return hd
        [d| instance ClosureDict $constraintQ => ClosureDict $hdQ where
              closureDict =
                  closure (static (mapDict (Sub Dict)))
                    `cap` (closureDict :: Closure (Dict $constraintQ)) |]
    go hd@(TH.AppT _ _) = go (TH.ForallT [] [] hd)
