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
  , renameVarsBy
  )
where

import Data.Haexpress.Core
import Data.Haexpress.Utils.List
import Data.Maybe (fromMaybe)

-- | /O(n*m)/.
-- Applies a function to all terminal values in an expression.
-- (cf. '//-')
--
-- Given that:
--
-- > > let zero  = val (0 :: Int)
-- > > let one   = val (1 :: Int)
-- > > let two   = val (2 :: Int)
-- > > let three = val (3 :: Int)
-- > > let xx -+- yy = value "+" ((+) :: Int->Int->Int) :$ xx :$ yy
-- > > let intToZero e = if typ e == typ zero then zero else e
--
-- Then:
--
-- > > one -+- (two -+- three)
-- > 1 + (2 + 3) :: Int
--
-- > > mapValues intToZero $ one -+- (two -+- three)
-- > 0 + (0 + 0) :: Integer
--
-- Given that the argument function is /O(m)/, this function is /O(n*m)/.
mapValues :: (Expr -> Expr) -> Expr -> Expr
mapValues f  =  m
  where
  m (e1 :$ e2)  =  m e1 :$ m e2
  m e           =  f e

-- | /O(n*m)/.
-- Applies a function to all variables in an expression.
--
-- Given that:
--
-- > > let primeify e = if isVar e
-- > |                  then case e of (Value n d) -> Value (n ++ "'") d
-- > |                  else e
-- > > let xx = var "x" (undefined :: Int)
-- > > let yy = var "y" (undefined :: Int)
-- > > let xx -+- yy = value "+" ((+) :: Int->Int->Int) :$ xx :$ yy
--
-- Then:
--
-- > > xx -+- yy
-- > x + y :: Int
--
-- > > primeify xx
-- > x' :: Int
--
-- > > mapVars primeify $ xx -+- yy
-- > x' + y' :: Int
--
-- > > mapVars (primeify . primeify) $ xx -+- yy
-- > x'' + y'' :: Int
--
-- Given that the argument function is /O(m)/, this function is /O(n*m)/.
mapVars :: (Expr -> Expr) -> Expr -> Expr
mapVars f  =  mapValues f'
  where
  f' e  =  if isVar e
           then f e
           else e

-- | /O(n*m)/.
-- Applies a function to all terminal constants in an expression.
--
-- Given that:
--
-- > > let one   = val (1 :: Int)
-- > > let two   = val (2 :: Int)
-- > > let xx -+- yy = value "+" ((+) :: Int->Int->Int) :$ xx :$ yy
-- > > let intToZero e = if typ e == typ zero then zero else e
--
-- Then:
--
-- > > one -+- (two -+- xx)
-- > 1 + (2 + x) :: Int
--
-- > > mapConsts intToZero (one -+- (two -+- xx))
-- > 0 + (0 + x) :: Integer
--
-- Given that the argument function is /O(m)/, this function is /O(n*m)/.
mapConsts :: (Expr -> Expr) -> Expr -> Expr
mapConsts f  =  mapValues f'
  where
  f' e  =  if isConst e
           then f e
           else e

-- | /O(n*m)/.
-- Substitute subexpressions of an expression using the given function.
-- Outer expressions have more precedence than inner expressions.
-- (cf. '//')
--
-- With:
--
-- > > let xx = var "x" (undefined :: Int)
-- > > let yy = var "y" (undefined :: Int)
-- > > let zz = var "z" (undefined :: Int)
-- > > let plus = value "+" ((+) :: Int->Int->Int)
-- > > let times = value "*" ((*) :: Int->Int->Int)
-- > > let xx -+- yy = plus :$ xx :$ yy
-- > > let xx -*- yy = times :$ xx :$ yy
--
-- > > let pluswap (o :$ xx :$ yy) | o == plus = Just $ o :$ yy :$ xx
-- > |     pluswap _                           = Nothing
--
-- Then:
--
-- > > mapSubexprs pluswap $ (xx -*- yy) -+- (yy -*- zz)
-- > y * z + x * y :: Int
--
-- > > mapSubexprs pluswap $ (xx -+- yy) -*- (yy -+- zz)
-- > (y + x) * (z + y) :: Int
--
-- Substitutions do not stack, in other words
-- a replaced expression or its subexpressions are not further replaced:
--
-- > > mapSubexprs pluswap $ (xx -+- yy) -+- (yy -+- zz)
-- > (y + z) + (x + y) :: Int
--
-- Given that the argument function is /O(m)/, this function is /O(n*m)/.
mapSubexprs :: (Expr -> Maybe Expr) -> Expr -> Expr
mapSubexprs f  =  m
  where
  m e  =  fromMaybe e' (f e)
    where
    e'  =  case e of
           e1 :$ e2 -> m e1 :$ m e2
           e -> e

-- | /O(n*m)/.
-- Substitute occurrences of values in an expression
-- from the given list of substitutions.
-- (cf. 'mapValues')
--
-- Given that:
--
-- > > let xx = var "x" (undefined :: Int)
-- > > let yy = var "y" (undefined :: Int)
-- > > let zz = var "z" (undefined :: Int)
-- > > let xx -+- yy = value "+" ((+) :: Int->Int->Int) :$ xx :$ yy
--
-- Then:
--
-- > > ((xx -+- yy) -+- (yy -+- zz)) //- [(xx, yy), (zz, yy)]
-- > (y + y) + (y + y) :: Int
--
-- > > ((xx -+- yy) -+- (yy -+- zz)) //- [(yy, yy -+- zz)]
-- > (x + (y + z)) + ((y + z) + z) :: Int
--
-- This function does not work for substituting non-terminal subexpressions:
--
-- > > (xx -+- yy) //- [(xx -+- yy, zz)]
-- > x + y :: Int
--
-- Please use the slower '//' if you want the above replacement to work.
--
-- Replacement happens only once:
--
-- > > xx //- [(xx,yy), (yy,zz)]
-- > y :: Int
--
-- Given that the argument list has length /m/,
-- this function is /O(n*m)/.
(//-) :: Expr -> [(Expr,Expr)] -> Expr
e //- s  =  mapValues (flip lookupId s) e

-- | /O(n*n*m)/.
-- Substitute subexpressions in an expression
-- from the given list of substitutions.
-- (cf. 'mapSubexprs').
--
-- Please consider using '//-' if you are replacing just terminal values
-- as it is faster.
--
-- Given that:
--
-- > > let xx = var "x" (undefined :: Int)
-- > > let yy = var "y" (undefined :: Int)
-- > > let zz = var "z" (undefined :: Int)
-- > > let xx -+- yy = value "+" ((+) :: Int->Int->Int) :$ xx :$ yy
--
-- Then:
--
-- > > ((xx -+- yy) -+- (yy -+- zz)) // [(xx -+- yy, yy), (yy -+- zz, yy)]
-- > y + y :: Int
--
-- > > ((xx -+- yy) -+- zz) // [(xx -+- yy, zz), (zz, xx -+- yy)]
-- > z + (x + y) :: Int
--
-- Replacement happens only once with outer expressions
-- having more precedence than inner expressions.
--
-- > > (xx -+- yy) // [(yy,xx), (xx -+- yy,zz), (zz,xx)]
-- > z :: Int
--
-- Given that the argument list has length /m/, this function is /O(n*n*m)/.
-- Remember that since /n/ is the size of an expression,
-- comparing two expressions is /O(n)/ in the worst case,
-- and we may need to compare with /n/ subexpressions in the worst case.
(//) :: Expr -> [(Expr,Expr)] -> Expr
e // s  =  mapSubexprs (flip lookup s) e

-- | Rename variables in an 'Expr'.
--
-- > > renameVarsBy (++ "'") (xx -+- yy)
-- > x' + y' :: Int
--
-- > > renameVarsBy (++ "'") (yy -+- (zz -+- xx))
-- > (y' + (z' + x')) :: Int
--
-- > > renameVarsBy (++ "1") (abs' xx)
-- > abs x1 :: Int
--
-- > > renameVarsBy (++ "2") $ abs' (xx -+- yy)
-- > abs (x2 + y2) :: Int
--
-- NOTE: this will affect holes!
renameVarsBy :: (String -> String) -> Expr -> Expr
renameVarsBy f = mapValues f'
  where
  f' (Value ('_':n) t) = Value ('_':f n) t
  f' e = e
