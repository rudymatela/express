{-# LANGUAGE TemplateHaskell, CPP #-}
-- |
-- Module      : Data.Express.Express.Derive
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Allows automatic derivation of 'Express' typeclass instances.
module Data.Express.Express.Derive
  ( deriveExpress
  , deriveExpressCascading
  , deriveExpressIfNeeded
  )
where

import Data.Express.Core
import Data.Express.Express

import Control.Monad
import Data.Char
import Data.List
import Data.Express.Utils.TH
import Data.Express.Utils.List
import Data.Express.Utils.String

-- | Derives an 'Express' instance for the given type 'Name'.
--
-- This function needs the @TemplateHaskell@ extension.
--
-- If '-:', '->:', '->>:', '->>>:', ... are not in scope,
-- this will derive them as well.
deriveExpress :: Name -> DecsQ
deriveExpress  =  deriveWhenNeededOrWarn ''Express reallyDeriveExpress

-- | Same as 'deriveExpress' but does not warn when instance already exists
--   ('deriveExpress' is preferable).
deriveExpressIfNeeded :: Name -> DecsQ
deriveExpressIfNeeded  =  deriveWhenNeeded ''Express reallyDeriveExpress

-- | Derives a 'Express' instance for a given type 'Name'
--   cascading derivation of type arguments as well.
deriveExpressCascading :: Name -> DecsQ
deriveExpressCascading  =  deriveWhenNeeded ''Express reallyDeriveExpressCascading

reallyDeriveExpress :: Name -> DecsQ
reallyDeriveExpress t  =  do
  isEq <- t `isInstanceOf` ''Eq
  isOrd <- t `isInstanceOf` ''Ord
  (nt,vs) <- normalizeType t
#if __GLASGOW_HASKELL__ >= 710
  cxt <- sequence [ [t| $(conT c) $(return v) |]
#else
  -- template-haskell <= 2.9.0.0:
  cxt <- sequence [ classP c [return v]
#endif
                  | c <- ''Express:([''Eq | isEq] ++ [''Ord | isOrd])
                  , v <- vs]
  cs <- typeConstructorsArgNames t
  asName <- newName "x"
  let withTheReturnTypeOfs = deriveWithTheReturnTypeOfs $ [length ns | (_,ns) <- cs]
  let generalizableExpr = mergeIFns $ foldr1 mergeI
        [ do let retTypeOf = mkName $ "-" ++ replicate (length ns) '>' ++ ":"
             let exprs = [[| expr $(varE n) |] | n <- ns]
             let conex = [| $(varE retTypeOf) $(conE c) $(varE asName) |]
             let root = [| value $(stringE $ showJustName c) $(conex) |]
             let rhs = foldl (\e1 e2 -> [| $e1 :$ $e2 |]) root exprs
             [d| instance Express $(return nt) where
                   expr $(asP asName $ conP c (map varP ns)) = $rhs |]
        | (c,ns) <- cs
        ]
  withTheReturnTypeOfs |++| (cxt |=>| generalizableExpr)

-- Not only really derive Express instances,
-- but cascade through argument types.
reallyDeriveExpressCascading :: Name -> DecsQ
reallyDeriveExpressCascading  =  reallyDeriveCascading ''Express reallyDeriveExpress

deriveWithTheReturnTypeOfs :: [Int] -> DecsQ
deriveWithTheReturnTypeOfs  =
  fmap concat . mapM deriveWithTheReturnTypeOf . nubSort

deriveWithTheReturnTypeOf :: Int -> DecsQ
deriveWithTheReturnTypeOf n  =  do
  mf <- lookupValueName name
  case mf of
    Nothing -> reallyDeriveWithTheReturnTypeOf n
    Just _  -> return []
  where
  name  =  "-" ++ replicate n '>' ++ ":"

reallyDeriveWithTheReturnTypeOf :: Int -> DecsQ
reallyDeriveWithTheReturnTypeOf n  =  do
  td <- sigD name theT
  vd <- [d| $(varP name) = const |]
  return $ td:vd
  where
  theT  =  [t| $(theFunT) -> $(last vars) -> $(theFunT) |]
  theFunT  =  foldr1 funT vars
  funT t1 t2  =  [t| $(t1) -> $(t2) |]
  vars  =  map (varT . mkName) . take (n+1) . primeCycle $ map (:"") ['a'..'z']
  name  =  mkName $ "-" ++ replicate n '>' ++ ":"
