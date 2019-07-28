{-# LANGUAGE TemplateHaskell, CPP #-}
-- |
-- Module      : Data.Haexpress.Express.Derive
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Allows automatic derivation of 'Express' typeclass instances.
module Data.Haexpress.Express.Derive
  ( deriveExpress
  , deriveExpressCascading
  , deriveExpressIfNeeded
  )
where

import Data.Haexpress.Core
import Data.Haexpress.Express

import Control.Monad
import Data.Char
import Data.List
import Data.Haexpress.Utils.TH

-- | Derives an 'Express' instance for the given type 'Name'.
--
-- This function needs the @TemplateHaskell@ extension.
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
  let generalizableExpr = mergeIFns $ foldr1 mergeI
        [ do retTypeOf <- lookupValN $ "-" ++ replicate (length ns) '>' ++ ":"
             let exprs = [[| expr $(varE n) |] | n <- ns]
             let conex = [| $(varE retTypeOf) $(conE c) $(varE asName) |]
             let root = [| value $(stringE $ showJustName c) $(conex) |]
             let rhs = foldl (\e1 e2 -> [| $e1 :$ $e2 |]) root exprs
             [d| instance Express $(return nt) where
                   expr $(asP asName $ conP c (map varP ns)) = $rhs |]
        | (c,ns) <- cs
        ]
  cxt |=>| generalizableExpr

-- Not only really derive Express instances,
-- but cascade through argument types.
reallyDeriveExpressCascading :: Name -> DecsQ
reallyDeriveExpressCascading  =  reallyDeriveCascading ''Express reallyDeriveExpress
