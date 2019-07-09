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
    module Data.Haexpress.Basic
  , module Data.Haexpress.Canon
  , module Data.Haexpress.Match
  , module Data.Haexpress.Instances
  , module Data.Haexpress.Name
  , module Data.Haexpress.Express
  , isSubexprOf
  )
where

import Data.Haexpress.Basic
import Data.Haexpress.Canon
import Data.Haexpress.Match
import Data.Haexpress.Instances
import Data.Haexpress.Name
import Data.Haexpress.Express

-- | /O(n^2)/.
-- Checks if an 'Expr' is a subexpression of another.
--
-- > > (xx -+- yy) `isSubexpr` (zz -+- (xx -+- yy))
-- > True
--
-- > > (xx -+- yy) `isSubexpr` abs' (yy -+- xx)
-- > False
--
-- > > xx `isSubexpr` yy
-- > False
isSubexprOf :: Expr -> Expr -> Bool
isSubexprOf e = (e `elem`) . subexprs
-- TODO: test isSubexprOf
