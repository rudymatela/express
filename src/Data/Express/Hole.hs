-- |
-- Module      : Data.Express.Hole
-- Copyright   : (c) 2019-2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Utilities for manipulating variables and typed holes encoded as 'Expr's.
{-# LANGUAGE CPP #-}
module Data.Express.Hole
  (
  -- * Creating variables
    varAsTypeOf
  , listVars
  , listVarsAsTypeOf

  -- * Typed holes
  , hole
  , isHole
  , hasHole
  , isComplete
  , holes
  , nubHoles
  , holeAsTypeOf
  , fill
  )
where

import Data.Express.Core

import Data.Dynamic
import Data.Maybe (fromMaybe)
import Data.Express.Utils.Typeable (tyArity)
import Data.Express.Utils.List (nubSort)
import Data.Express.Utils.String (variableNamesFromTemplate)

-- | /O(1)/.
-- Creates a 'var'iable with the same type as the given 'Expr'.
--
-- > > let one = val (1::Int)
-- > > "x" `varAsTypeOf` one
-- > x :: Int
--
-- > > "p" `varAsTypeOf` val False
-- > p :: Bool
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

-- | /O(1)/.
-- Creates an 'Expr' representing a typed hole with the type of the given
-- 'Expr'. (cf. 'hole')
--
-- > > val (1::Int)
-- > 1 :: Int
-- > > holeAsTypeOf $ val (1::Int)
-- > _ :: Int
holeAsTypeOf :: Expr -> Expr
holeAsTypeOf = ("" `varAsTypeOf`)

-- | /O(1)/.
-- Creates an 'Expr' representing a typed hole of the given argument type.
--
-- > > hole (undefined :: Int)
-- > _ :: Int
--
-- > > hole (undefined :: Maybe String)
-- > _ :: Maybe [Char]
--
-- A hole is represented as a variable with no name or
-- a value named @"_"@:
--
-- > hole x = var "" x
-- > hole x = value "_" x
hole :: Typeable a => a -> Expr
hole a = var "" (undefined `asTypeOf` a)

-- | /O(1)/.
-- Checks if an 'Expr' represents a typed hole.
-- (cf. 'hole')
--
-- > > isHole $ hole (undefined :: Int)
-- > True
--
-- > > isHole $ value "not" not :$ val True
-- > False
--
-- > > isHole $ val 'a'
-- > False
isHole :: Expr -> Bool
isHole (Value "_" _)  = True
isHole _              = False

-- | /O(n)/.
-- Lists all holes in an expression, in order and with repetitions.
-- (cf. 'nubHoles')
--
-- > > holes $ hole (undefined :: Bool)
-- > [_ :: Bool]
--
-- > > holes $ value "&&" (&&) :$ hole (undefined :: Bool) :$ hole (undefined :: Bool)
-- > [_ :: Bool,_ :: Bool]
--
-- > > holes $ hole (undefined :: Bool->Bool) :$ hole (undefined::Bool)
-- > [_ :: Bool -> Bool,_ :: Bool]
holes :: Expr -> [Expr]
holes  =  filter isHole . values

-- | /O(n^2)/.
-- Lists all holes in an expression without repetitions.
-- (cf. 'holes')
--
-- > > nubHoles $ hole (undefined :: Bool)
-- > [_ :: Bool]
--
-- > > nubHoles $ value "&&" (&&) :$ hole (undefined :: Bool) :$ hole (undefined :: Bool)
-- > [_ :: Bool]
--
-- > > nubHoles $ hole (undefined :: Bool->Bool) :$ hole (undefined::Bool)
-- > [_ :: Bool,_ :: Bool -> Bool]
--
-- Runtime averages to
-- /O(n log n)/ on evenly distributed expressions
-- such as @(f x + g y) + (h z + f w)@;
-- and to /O(n^2)/ on deep expressions
-- such as @f (g (h (f (g (h x)))))@.
nubHoles :: Expr -> [Expr]
nubHoles  =  nubSort . holes

-- | /O(n)/.
-- Returns whether an expression contains a hole
--
-- > > hasHole $ hole (undefined :: Bool)
-- > True
--
-- > > hasHole $ value "not" not :$ val True
-- > False
--
-- > > hasHole $ value "not" not :$ hole (undefined :: Bool)
-- > True
hasHole :: Expr -> Bool
hasHole  =  any isHole . values

-- | /O(n)/.
-- Returns whether an expression is complete.
-- A complete expression is one without holes.
--
-- > > isComplete $ hole (undefined :: Bool)
-- > False
--
-- > > isComplete $ value "not" not :$ val True
-- > True
--
-- > > isComplete $ value "not" not :$ hole (undefined :: Bool)
-- > False
--
-- 'isComplete' is the negation of 'hasHole'.
--
-- > isComplete  =  not . hasHole
--
-- 'isComplete' is to 'hasHole' what
-- 'isGround' is to 'hasVar'.
isComplete :: Expr -> Bool
isComplete  =  not . hasHole

-- |
-- Generate an infinite list of variables
-- based on a template and a given type.
-- (cf. 'listVarsAsTypeOf')
--
-- > > putL 10 $ listVars "x" (undefined :: Int)
-- > [ x :: Int
-- > , y :: Int
-- > , z :: Int
-- > , x' :: Int
-- > , y' :: Int
-- > , z' :: Int
-- > , x'' :: Int
-- > , ...
-- > ]
--
-- > > putL 10 $ listVars "p" (undefined :: Bool)
-- > [ p :: Bool
-- > , q :: Bool
-- > , r :: Bool
-- > , p' :: Bool
-- > , q' :: Bool
-- > , r' :: Bool
-- > , p'' :: Bool
-- > , ...
-- > ]
listVars :: Typeable a => String -> a -> [Expr]
listVars s a  =  map (`var` a) (variableNamesFromTemplate s)

-- |
-- Generate an infinite list of variables
-- based on a template
-- and the type of a given 'Expr'.
-- (cf. 'listVars')
--
-- > > let one = val (1::Int)
-- > > putL 10 $ "x" `listVarsAsTypeOf` one
-- > [ x :: Int
-- > , y :: Int
-- > , z :: Int
-- > , x' :: Int
-- > , ...
-- > ]
--
-- > > let false = val False
-- > > putL 10 $ "p" `listVarsAsTypeOf` false
-- > [ p :: Bool
-- > , q :: Bool
-- > , r :: Bool
-- > , p' :: Bool
-- > , ...
-- > ]
listVarsAsTypeOf :: String -> Expr -> [Expr]
listVarsAsTypeOf s e  =  map (`varAsTypeOf` e) (variableNamesFromTemplate s)


-- | Fill holes in an expression with the given list.
--
-- > > let i_  =  hole (undefined :: Int)
-- > > let e1 -+- e2  =  value "+" ((+) :: Int -> Int -> Int) :$ e1 :$ e2
-- > > let xx  =  var "x" (undefined :: Int)
-- > > let yy  =  var "y" (undefined :: Int)
--
-- > > fill (i_ -+- i_) [xx, yy]
-- > x + y :: Int
--
-- > > fill (i_ -+- i_) [xx, xx]
-- > x + x :: Int
--
-- > > let one  =  val (1::Int)
--
-- > > fill (i_ -+- i_) [one, one -+- one]
-- > 1 + (1 + 1) :: Int
--
-- This function silently remaining expressions:
--
-- > > fill i_ [xx, yy]
-- > x :: Int
--
-- This function silently keeps remaining holes:
--
-- > > fill (i_ -+- i_ -+- i_) [xx, yy]
-- > (x + y) + _ :: Int
--
-- This function silently skips remaining holes
-- if one is not of the right type:
--
-- > > fill (i_ -+- i_ -+- i_) [xx, val 'c', yy]
-- > (x + _) + _ :: Int
fill :: Expr -> [Expr] -> Expr
fill e = fst . fill' e
  where
  fill' :: Expr -> [Expr] -> (Expr,[Expr])
  fill' (e1 :$ e2) es = let (e1',es')  = fill' e1 es
                            (e2',es'') = fill' e2 es'
                        in (e1' :$ e2', es'')
  fill' eh (e:es) | isHole eh && typ eh == typ e = (e,es)
  fill' e es = (e,es)
