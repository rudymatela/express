-- |
-- Module      : Data.Haexpress.Fold
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines utilities for folding and unfolding 'Expr's.
module Data.Haexpress.Fold
  ( fold
  , unfold
  , foldPair
  , unfoldPair
  , foldTrio
  , unfoldTrio
  , foldApp
  , unfoldApp
  )
where

import Data.Haexpress.Core

data ExprPair = ExprPair

-- | /O(n)/.
-- Folds a list of 'Expr' with function application (':$').
-- This reverses the effect of 'unfoldApp'.
--
-- > foldApp [e0]           =  e0
-- > foldApp [e0,e1]        =  e0 :$ e1
-- > foldApp [e0,e1,e2]     =  e0 :$ e1 :$ e2
-- > foldApp [e0,e1,e2,e3]  =  e0 :$ e1 :$ e2 :$ e3
--
-- Remember ':$' is left-associative, so:
--
-- > foldApp [e0]           =    e0
-- > foldApp [e0,e1]        =   (e0 :$ e1)
-- > foldApp [e0,e1,e2]     =  ((e0 :$ e1) :$ e2)
-- > foldApp [e0,e1,e2,e3]  = (((e0 :$ e1) :$ e2) :$ e3)
--
-- This function /may/ produce an ill-typed expression.
foldApp :: [Expr] -> Expr
foldApp = foldl1 (:$)

-- | /O(1)/.
-- Folds a pair of 'Expr' values into a single 'Expr'.
-- (cf. 'unfoldPair')
--
-- This /always/ generates an ill-typed expression.
--
-- > > foldPair (val False, val (1::Int))
-- > (False,1) :: ill-typed # ExprPair $ Bool #
--
-- > > foldPair (val (0::Int), val True)
-- > (0,True) :: ill-typed # ExprPair $ Int #
--
-- This is useful when applying transformations on pairs of 'Expr's, such as
-- 'Data.Haexpress.Canon.canonicalize',
-- 'Data.Haexpress.Map.mapValues' or
-- 'Data.Haexpress.Canon.canonicalVariations'.
--
-- > > let ii = var "i" (undefined::Int)
-- > > let kk = var "k" (undefined::Int)
-- > > unfoldPair $ canonicalize $ foldPair (ii,kk)
-- > (x :: Int,y :: Int)
foldPair :: (Expr,Expr) -> Expr
foldPair (e1,e2)  =  value "," (undefined :: ExprPair) :$ e1 :$ e2

-- | /O(1)/.
-- Unfolds an 'Expr' representing a pair.
-- This reverses the effect of 'foldPair'.
--
-- > > value "," ((,) :: Bool->Bool->(Bool,Bool)) :$ val True :$ val False
-- > (True,False) :: (Bool,Bool)
-- > > unfoldPair $ value "," ((,) :: Bool->Bool->(Bool,Bool)) :$ val True :$ val False
-- > (True :: Bool,False :: Bool)
unfoldPair :: Expr -> (Expr,Expr)
unfoldPair (Value "," _ :$ e1 :$ e2) = (e1,e2)
unfoldPair (Value "(,)" _ :$ e1 :$ e2) = (e1,e2)
unfoldPair _  =  error "unfoldPair: not an Expr pair"

data ExprTrio = ExprTrio

foldTrio :: (Expr,Expr,Expr) -> Expr
foldTrio (e1,e2,e3)  =  value ",," (undefined :: ExprTrio) :$ e1 :$ e2 :$ e3

unfoldTrio :: Expr -> (Expr,Expr,Expr)
unfoldTrio (Value ",," _ :$ e1 :$ e2 :$ e3) = (e1,e2,e3)
unfoldTrio (Value "(,,)" _ :$ e1 :$ e2 :$ e3) = (e1,e2,e3)
unfoldTrio _  =  error "unfoldTrio: not an Expr trio"

data ExprList = ExprList

-- | /O(n)/.
-- Folds a list of 'Expr's into a single 'Expr'.
-- (cf. 'unfold')
--
-- This /always/ generates an ill-typed expression.
--
-- > fold [val False, val True, val (1::Int)]
-- > [False,True,1] :: ill-typed # ExprList $ Bool #
--
-- This is useful when applying transformations on lists of 'Expr's, such as
-- 'Data.Haexpress.Canon.canonicalize',
-- 'Data.Haexpress.Map.mapValues' or
-- 'Data.Haexpress.Canon.canonicalVariations'.
--
-- > > let ii = var "i" (undefined::Int)
-- > > let kk = var "k" (undefined::Int)
-- > > let qq = var "q" (undefined::Bool)
-- > > let notE = value "not" not
-- > > unfold . canonicalize . fold $ [ii,kk,notE :$ qq, notE :$ val False]
-- > [x :: Int,y :: Int,not p :: Bool,not False :: Bool]
fold :: [Expr] -> Expr
fold []      =  value "[]" ExprList
fold (e:es)  =  value ":"  ExprList :$ e :$ fold es

-- | /O(n)/.
-- Unfolds an 'Expr' representing a list into a list of 'Expr's.
-- This reverses the effect of 'fold'.
--
-- > > expr [1,2,3::Int]
-- > [1,2,3] :: [Int]
-- > > unfold $ expr [1,2,3::Int]
-- > [1 :: Int,2 :: Int,3 :: Int]
unfold :: Expr -> [Expr]
unfold   (Value "[]" _)               =  []
unfold (((Value ":"  _) :$ e) :$ es)  =  e : unfold es
unfold e  =  error $ "unfold: cannot unfold expression: " ++ show e
