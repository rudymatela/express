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
  , (//-)
  , (//)
  )
where

import Data.Haexpress.Core
import Data.List (find)
import Data.Maybe (fromMaybe)

-- TODO: implement mapValues
-- TODO: implement mapVars and use it on //-
-- TODO: implement mapConsts
-- TODO: implement mapOuter
-- TODO: implement mapMaybeOuter :: (Expr -> Maybe Expr) -> Expr -> Expr
--                 and use it on //, maybe ...
-- TODO: implement mapInner


-- | /O(n)/.
-- Applies a function to all terminal values in an expression.
mapValues :: (Expr -> Expr) -> Expr -> Expr
mapValues f  =  m
  where
  m (e1 :$ e2)  =  m e1 :$ m e2
  m e  =  f e

-- | /O(n)/.
-- Applies a function to all variables in an expression.
mapVars :: (Expr -> Expr) -> Expr -> Expr
mapVars f  =  mapValues f'
  where
  f' e = if isVar e
         then f e
         else e

-- | /O(n)/.
-- Applies a function to all terminal constants in an expression.
mapConsts :: (Expr -> Expr) -> Expr -> Expr
mapConsts f  =  mapValues f'
  where
  f' e = if isConst e
         then f e
         else e

-- | /O(n+m*v)/.
-- Substitute all occurrences of variables in an expression.
--
-- > > ((xx -+- yy) -+- (yy -+- zz)) // [(yy, yy -+- zz)] =
-- > (x + (y + z)) + ((y + z) + z)
(//-) :: Expr -> [(Expr,Expr)] -> Expr
e //- s  =  mapVars (// s) e

-- | /O(n+n*m)/.
-- Substitute subexpressions in an expression.
-- Larger expressions take more precedence.  <-- TODO: explain this
(//) :: Expr -> [(Expr,Expr)] -> Expr
e // s  =  fromMaybe r $ snd <$> find ((== e) . fst) s
  where
  r = case e of
      (e1 :$ e2) -> (e1 // s) :$ (e2 // s)
      e          -> e
