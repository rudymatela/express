-- |
-- Module      : Data.Haexpress.Fold
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines utilities for folding and unfolding 'Expr's.
module Data.Haexpress.Fold
  ( pair
  , unpair
  , unfoldApp
  )
where

-- TODO: isList
-- TODO: unfoldList

import Data.Haexpress.Core

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
