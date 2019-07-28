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

updateAssignments :: (Expr,Expr) -> [(Expr,Expr)] -> Maybe [(Expr,Expr)]
updateAssignments (e,e') = \bs ->
  case lookup e bs of
    Nothing  -> Just ((e,e'):bs)
    Just e'' -> if e'' == e'
                then Just bs
                else Nothing

-- |
-- Given two 'Expr's,
-- checks if the first expression
-- is an instance of the second
-- in terms of variables.
-- (cf. 'hasInstanceOf')
--
-- > > let zero = val (0::Int)
-- > > let one  = val (1::Int)
-- > > let xx   = var "x" (undefined :: Int)
-- > > let yy   = var "y" (undefined :: Int)
-- > > let e1 -+- e2  =  value "+" ((+)::Int->Int->Int) :$ e1 :$ e2
--
-- >  one `isInstanceOf` one   =  True
-- >   xx `isInstanceOf` xx    =  True
-- >   yy `isInstanceOf` xx    =  True
-- > zero `isInstanceOf` xx    =  True
-- >   xx `isInstanceOf` zero  =  False
-- >  one `isInstanceOf` zero  =  False
-- >   (xx -+- (yy -+- xx)) `isInstanceOf`   (xx -+- yy)  =  True
-- >   (yy -+- (yy -+- xx)) `isInstanceOf`   (xx -+- yy)  =  True
-- > (zero -+- (yy -+- xx)) `isInstanceOf` (zero -+- yy)  =  True
-- >  (one -+- (yy -+- xx)) `isInstanceOf` (zero -+- yy)  =  False
isInstanceOf :: Expr -> Expr -> Bool
e1 `isInstanceOf` e2 = isJust $ e1 `match` e2

-- |
-- Checks if any of the subexpressions of the first argument 'Expr'
-- is an instance of the second argument 'Expr'.
hasInstanceOf :: Expr -> Expr -> Bool
e1 `hasInstanceOf` e2  =  any (`isInstanceOf` e2) (subexprs e1)

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
