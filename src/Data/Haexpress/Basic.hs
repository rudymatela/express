-- |
-- Module      : Data.Haexpress.Basic
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Expr' type and _basic_ utilities involving it, including:
--
-- * re-export of "Data.Haexpress.Core"
-- * re-export of "Data.Haexpress.Map"
-- * utilities for folding and unfolding 'Expr's into lists and tuples
-- * utilities for creating and manipulating variables and typed holes
--
-- If you're a Haexpress user,
-- you're probably better of importing "Data.Haexpress".
{-# LANGUAGE CPP #-}
module Data.Haexpress.Basic
  (
  -- * Module re-exports
    module Data.Haexpress.Core
  , module Data.Haexpress.Map

  -- * Creating variables
  , varAsTypeOf

  -- * Typed holes
  -- TODO: TBA

  -- * Folding and unfolding
  , pair
  , unpair
  )
where

import Data.Haexpress.Core
import Data.Haexpress.Map

import Data.Dynamic
import Data.Maybe (fromMaybe)
import Data.Haexpress.Utils.Typeable (tyArity)

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

data ExprPair = ExprPair

-- note this will generate an ill-typed pair expression
-- use with caution
-- uses: e.g.: unpair . canonicalize . pair
pair :: Expr -> Expr -> Expr
pair e1 e2  =  value "," (undefined :: ExprPair) :$ e1 :$ e2
-- TODO: document & test pair
-- TODO: rename the function above to foldPair?

-- note this is intended to undo the effect of pair
unpair :: Expr -> (Expr,Expr)
unpair (Value "," _ :$ e1 :$ e2) = (e1,e2)
unpair _  =  error "unpair: not an Expr pair"
-- TODO: rename the function above to unfoldPair?
-- TODO: document & test unpair

-- TODO: remove the following comment section eventually
--
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
