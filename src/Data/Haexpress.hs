-- |
-- Module      : Data.Haexpress
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Expr' type and utilities involving it
{-# LANGUAGE CPP #-}
module Data.Haexpress
  ( 
-- TODO: explicitly export everything instead of the modules
    module Data.Haexpress.Core
  , module Data.Haexpress.Map
  , module Data.Haexpress.Instances
  , module Data.Haexpress.Name
  , module Data.Haexpress.Express
  , varAsTypeOf
  , arity
  , size
  , pair
  )
where

import Data.Haexpress.Core
import Data.Haexpress.Map
import Data.Haexpress.Instances
import Data.Haexpress.Name
import Data.Haexpress.Express

-- TODO: move stuff below into submodules of its own:

import Data.Dynamic
import Data.Maybe (fromMaybe)
import Data.Haexpress.Utils.Typeable (tyArity)

-- implement \\\\ which works for all types of subterms, not only terminal ones

-- | /O(1)/.
-- Creates a 'var'iable with the same type as the given 'Expr'.
--
-- > > let one = val (1::Int)
-- > > "x" `varAsTypeOf` one
-- > x :: Int
varAsTypeOf :: String -> Expr -> Expr
varAsTypeOf n = Value ('_':n) . undefine . fromMaybe err . toDynamic
  where
  err = error "varAsTypeOf: could not compile Dynamic value, type error?"
  undefine :: Dynamic -> Dynamic
#if __GLASGOW_HASKELL__ >= 806
  undefine (Dynamic t v) = (Dynamic t undefined)
#else
  undefine = id -- there's no way to do this using the old Data.Dynamic API.
#endif

-- | /O(n)/?
isSubExprOf :: Expr -> Expr -> Bool
e `isSubExprOf` e0 | e == e0  =  True
e `isSubExprOf` (e1 :$ e2)    =  e `isSubExprOf` e1
                              || e `isSubExprOf` e2
e `isSubExprOf` _             =  False

arity :: Expr -> Int
arity  =  tyArity . typ
-- TODO: document & test arity

size :: Expr -> Int
size  =  length . values
-- TODO: document & test size

data ExprPair = ExprPair

-- note this will generate an ill-typed pair expression
-- use with caution
-- uses: e.g.: unpair . canonicalize . pair
pair :: Expr -> Expr -> Expr
pair e1 e2  =  value "," (undefined :: ExprPair) :$ e1 :$ e2
-- TODO: document & test pair

-- Folds an expression with applications into a "value" expression.
--
-- > > let compareE = value "compare" (compare :: Bool -> Bool -> Ordering)
-- > > let lessThanOrEqualE = toValueExpr $ value undefined (\(?) p q -> p ? q == LE) == compareE
--
-- This is not gonna work,  I don't have means to produce the above during
-- speculation: (?), p, and q would have to be polymorphic.
--
-- So I don't think adding the following is worth it...
--
-- > toValueExpr :: String -> Expr -> Expr
-- > toValueExpr s  =  Value s . toDynamic

-- An alternative that works on Speculate:
-- I just need to apply (==LE), (==GT) and (==EQ) when evaluating my
-- properties and to replace (isLE :$ (compare :$ ...)) by ((<=) :$ ...)
-- before printing.

-- NOTE: The following two are impossible with GHC <= 8.0, base <= 4.9:
-- pair :: Expr -> Expr -> Expr
-- unpair :: Expr -> (Expr,Expr)
