-- |
-- Module      : Data.Haexpress.Match
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Utilities for matching 'Expr's with 'var'iables.
module Data.Haexpress.Match
  ( match
  , matchWith
  , isInstanceOf
  , hasInstanceOf
  , isSubexprOf
  )
where

import Data.Haexpress.Basic
import Data.Maybe
import Data.Functor ((<$>))
import Control.Monad ((>=>))

-- |
-- Given two expressions, returns a 'Just' list of matches
-- of subexpressions of the first expressions
-- to variables in the second expression.
-- Returns 'Nothing' when there is no match.
--
-- > > let zero = val (0::Int)
-- > > let one  = val (1::Int)
-- > > let xx   = var "x" (undefined :: Int)
-- > > let yy   = var "y" (undefined :: Int)
-- > > let e1 -+- e2  =  value "+" ((+)::Int->Int->Int) :$ e1 :$ e2
--
-- > > (zero -+- one) `match` (xx -+- yy)
-- > Just [(y :: Int,1 :: Int),(x :: Int,0 :: Int)]
--
-- > > (zero -+- (one -+- two)) `match` (xx -+- yy)
-- > Just [(y :: Int,1 + 2 :: Int),(x :: Int,0 :: Int)]
--
-- > > (zero -+- (one -+- two)) `match` (xx -+- (yy -+- yy))
-- > Nothing
--
-- In short:
--
-- >           (zero -+- one) `match` (xx -+- yy)           =  Just [(xx,zero), (yy,one)]
-- > (zero -+- (one -+- two)) `match` (xx -+- yy)           =  Just [(xx,zero), (yy,one-+-two)]
-- > (zero -+- (one -+- two)) `match` (xx -+- (yy -+- yy))  =  Nothing
match :: Expr -> Expr -> Maybe [(Expr,Expr)]
match = matchWith []

-- |
-- Like 'match' but allowing predefined bindings.
--
-- > matchWith [(xx,zero)] (zero -+- one) (xx -+- yy)  =  Just [(xx,zero), (yy,one)]
-- > matchWith [(xx,one)]  (zero -+- one) (xx -+- yy)  =  Nothing
matchWith :: [(Expr,Expr)] -> Expr -> Expr -> Maybe [(Expr,Expr)]
matchWith bs e1' e2' = m e1' e2' bs
  where
  m :: Expr -> Expr -> [(Expr,Expr)] -> Maybe [(Expr,Expr)]
  m (f1 :$ x1) (f2 :$ x2)             =  m f1 f2 >=> m x1 x2
  m e1 e2
    | isVar e2 && mtyp e1 == mtyp e2  =  updateAssignments (e2,e1)
    | e1 == e2                        =  Just
    | otherwise                       =  const Nothing
-- TODO: proper documentation for matchWith

updateAssignments :: (Expr,Expr) -> [(Expr,Expr)] -> Maybe [(Expr,Expr)]
updateAssignments (e,e') = \bs ->
  case lookup e bs of
    Nothing  -> Just ((e,e'):bs)
    Just e'' -> if e'' == e'
                then Just bs
                else Nothing

-- 0 `isInstanceOf` x = True
-- y `isInstanceOf` x = True
-- x `isInstanceOf` 0 = False
-- 1 `isInstanceOf` 0 = False
-- x + (y + x) `isInstanceOf` x + y = True
-- y + (y + x) `isInstanceOf` x + y = True
-- 0 + (y + x) `isInstanceOf` x + y = True
-- x `isInstanceOf` x = True
-- _ `isInstanceOf` x = True
isInstanceOf :: Expr -> Expr -> Bool
e1 `isInstanceOf` e2 = isJust $ e1 `match` e2

hasInstanceOf :: Expr -> Expr -> Bool
e1           `hasInstanceOf` e2 | e1   `isInstanceOf` e2 = True
(e1f :$ e1x) `hasInstanceOf` e2 | e1f `hasInstanceOf` e2 ||
                                  e1x `hasInstanceOf` e2 = True
_            `hasInstanceOf` _                           = False

-- | /O(n^2)/.
-- Checks if an 'Expr' is a subexpression of another.
--
-- > > (xx -+- yy) `isSubexprOf` (zz -+- (xx -+- yy))
-- > True
--
-- > > (xx -+- yy) `isSubexprOf` abs' (yy -+- xx)
-- > False
--
-- > > xx `isSubexprOf` yy
-- > False
isSubexprOf :: Expr -> Expr -> Bool
isSubexprOf e = (e `elem`) . subexprs
-- TODO: test isSubexprOf
