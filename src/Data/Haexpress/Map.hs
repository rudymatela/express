-- |
-- Module      : Data.Haexpress.Map
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Utilities for mapping or transforming 'Expr's.
module Data.Haexpress.Map
  ( mapValues
  , mapVars
  , mapConsts
  , mapSubexprs
  , (//-)
  , (//)
  )
where

import Data.Haexpress.Core
import Data.Haexpress.Utils.List
import Data.Maybe (fromMaybe)

-- TODO: implement mapOuter
-- TODO: implement mapMaybeOuter :: (Expr -> Maybe Expr) -> Expr -> Expr
--                 and use it on //, maybe ...
-- TODO: implement mapInner


-- | /O(n)/.
-- Applies a function to all terminal values in an expression.
--
-- Given that:
--
-- > > let plus = value "+" (+)
-- > > let intToZero e = if typ e == typ (val 0) then val 0 else e
--
-- Then:
--
-- > > plus :$ val 1 :$ (plus :$ val 2 :$ val 3)
-- > 1 + (2 + 3) :: Integer
--
-- > > mapValues intToZero (plus :$ val 1 :$ (plus :$ val 2 :$ val 3))
-- > 0 + (0 + 0) :: Integer
mapValues :: (Expr -> Expr) -> Expr -> Expr
mapValues f  =  m
  where
  m (e1 :$ e2)  =  m e1 :$ m e2
  m e           =  f e

-- | /O(n)/.
-- Applies a function to all variables in an expression.
--
-- Given that:
--
-- > > let primeify e = if isVar e
-- > |                  then case e of (Value n d) -> Value (n ++ "'") d
-- > |                  else e
-- > > let xx = var "x" (undefined :: Int)
-- > > let yy = var "y" (undefined :: Int)
-- > > let plus = value "+" ((+) :: Int->Int->Int)
--
-- Then:
--
-- > > plus :$ xx :$ yy
-- > x + y :: Int
--
-- > > mapVars primeify $ plus :$ xx :$ yy
-- > x' + y' :: Int
--
-- > > mapVars (primeify . primeify) $ plus :$ xx :$ yy
-- > x'' + y'' :: Int
mapVars :: (Expr -> Expr) -> Expr -> Expr
mapVars f  =  mapValues f'
  where
  f' e  =  if isVar e
           then f e
           else e

-- | /O(n)/.
-- Applies a function to all terminal constants in an expression.
--
-- Given that:
--
-- > > let plus = value "+" (+)
-- > > let intToZero e = if typ e == typ (val 0) then val 0 else e
--
-- Then:
--
-- > > plus :$ val 1 :$ (plus :$ val 2 :$ var "x" (undefined :: Int))
-- > 1 + (2 + x) :: Integer
--
-- > > mapValues intToZero (plus :$ val 1 :$ (plus :$ val 2 :$ val 3))
-- > 0 + (0 + x) :: Integer
mapConsts :: (Expr -> Expr) -> Expr -> Expr
mapConsts f  =  mapValues f'
  where
  f' e  =  if isConst e
           then f e
           else e

-- | /O(n)/.
-- Substitute subexpressions of an expression.
-- Substitutions do not stack.
mapSubexprs :: (Expr -> Maybe Expr) -> Expr -> Expr
mapSubexprs f  =  m
  where
  m e  =  fromMaybe e' (f e)
    where
    e'  =  case e of
           e1 :$ e2 -> m e1 :$ m e2
           e -> e
-- TODO: explain non-stacking substitutions

-- | /O(n+m*v)/.
-- Substitute all occurrences of variables in an expression.
--
-- > > ((xx -+- yy) -+- (yy -+- zz)) // [(yy, yy -+- zz)] =
-- > (x + (y + z)) + ((y + z) + z)
(//-) :: Expr -> [(Expr,Expr)] -> Expr
e //- s  =  mapValues (flip lookupId s) e
-- TODO: explain complexity above

-- | /O(n+n*m)/.
-- Substitute subexpressions in an expression.
-- Larger expressions take more precedence.  <-- TODO: explain this
(//) :: Expr -> [(Expr,Expr)] -> Expr
e // s  =  mapSubexprs (flip lookup s) e
