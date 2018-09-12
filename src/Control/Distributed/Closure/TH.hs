{-# LANGUAGE CPP #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Utility Template Haskell macros.

module Control.Distributed.Closure.TH
  ( cstatic
  , cstaticDict
  , cdict
  , cdictFrom
  , withStatic
  ) where

import           Control.Monad (replicateM, unless)
import           Control.Distributed.Closure
import           Data.Generics (everything, mkQ)
import           Data.List (nub)
import           Data.Typeable (Typeable)
import           GHC.StaticPtr
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
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

-- | Compute free variables of a type.
fvT :: TH.Type -> [TH.Name]
fvT = nub . everything (++) ([] `mkQ` (\ty -> [nm | TH.VarT nm <- [ty]]))

caps :: [TH.ExpQ] -> TH.ExpQ
caps = foldl1 (\f x -> [| $f `cap` $x|])

-- XXX It turns out that GHC's newName doesn't produce really fresh names. Call
-- newName twice to define two new globals and you'll find they share the same
-- name. A workaround mentioned in https://ghc.haskell.org/trac/ghc/ticket/5398
-- is this snippet of code...
mangleName :: TH.Name -> TH.Name
mangleName name@(TH.Name occ fl) = case fl of
    TH.NameU u -> TH.Name (mangle_occ u) fl
    _ -> name
  where
    mangle_occ :: Int -> TH.OccName
    mangle_occ uniq = TH.mkOccName (TH.occString occ ++ "_" ++ show uniq)

-- | Auto-generates the 'Static' instances corresponding to the given class
-- instances. Example:
--
-- @
-- data T a = T a
--
-- withStatic [d| instance Show a => Show (T a) where ... |]
-- ======>
-- instance Show a => Show (T a) where ...
-- instance (Static (Show a), Typeable a) => Static (Show (T a)) where
--   closureDict = closure (static (Dict -> Dict)) `cap` closureDict
-- @
--
-- You will probably want to enable @FlexibleContexts@ and @ScopedTypeVariables@
-- in modules that use 'withStatic'. 'withStatic' can also handle non-user
-- generated instances like 'Typeable' instances: just write @instance Typeable
-- T@.
withStatic :: TH.DecsQ -> TH.DecsQ
withStatic = (>>= go)
  where
    checkExtension :: TH.Extension -> TH.Q ()
    checkExtension ext = do
      enabled <- TH.isExtEnabled TH.ScopedTypeVariables
      unless enabled $
        fail $ "withStatic requires the language extension " ++ show ext

    go :: [TH.Dec] -> TH.DecsQ
    go [] = return []
#if MIN_VERSION_template_haskell(2,11,0)
    go (ins@(TH.InstanceD overlap cxt hd _):decls) = do
#else
    go (ins@(TH.InstanceD cxt hd _):decls) = do
#endif
        let n = length cxt
        dictsigs <- mapM (\c -> [t| Dict $(return c) |]) cxt
        retsig <- [t| Dict $(return hd) |]
        f <- mangleName <$> TH.newName "static_helper"
        fbody <- foldr (\_ body -> [| \Dict -> $body |]) [| Dict |] cxt
        let tyf = foldr (\a b -> TH.ArrowT `TH.AppT` a `TH.AppT` b) retsig dictsigs
            sigf = TH.SigD f (TH.ForallT (map TH.PlainTV (fvT tyf)) [] tyf)
            declf = TH.ValD (TH.VarP f) (TH.NormalB fbody) []
        methods <- (:[]) <$>
          TH.valD
            (TH.varP 'closureDict)
            (TH.normalB (caps ( [| closure (static $(TH.varE f) :: StaticPtr $(return tyf)) |]
                              : replicate n [| closureDict |]
                              )))
            []
        typeableConstraints <-
          sequence [ [t| Typeable $(return d) |]
                   | d <- retsig : dictsigs
                   , not (null (fvT d))
                   ]
        unless (null typeableConstraints) $
          checkExtension TH.ScopedTypeVariables
        staticcxt <- (typeableConstraints ++) <$>
          mapM (\c -> [t| Static   $(return c) |]) cxt
        statichd <- [t| Static $(return hd) |]
#if MIN_VERSION_template_haskell(2,11,0)
        let staticins = TH.InstanceD overlap staticcxt statichd methods
#else
        let staticins = TH.InstanceD staticcxt statichd methods
#endif
        decls' <- go decls
        case hd of
          TH.AppT (TH.ConT nm) _ | nm == ''Typeable ->
            return (sigf : declf : staticins : decls')
          _ ->
            return (ins : sigf : declf : staticins : decls')
    go (decl:decls) = (decl:) <$> go decls
