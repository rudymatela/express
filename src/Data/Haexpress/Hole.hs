-- |
-- Module      : Data.Haexpress.Hole
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Utilities for manipulating variables and typed holes encoded as 'Expr's.
{-# LANGUAGE CPP #-}
module Data.Haexpress.Hole
  (
  -- * Creating variables
    varAsTypeOf
  , listVars
  , listVarsAsTypeOf

  -- * Typed holes
  , hole
  , isHole
  , holes
  , nubHoles
  , holeAsTypeOf
  )
where

import Data.Haexpress.Core

import Data.Dynamic
import Data.Maybe (fromMaybe)
import Data.Haexpress.Utils.Typeable (tyArity)
import Data.Haexpress.Utils.List (nubSort)
import Data.Haexpress.Utils.String (variableNamesFromTemplate)

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

-- | /O(n log n)/.
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
nubHoles :: Expr -> [Expr]
nubHoles  =  nubSort . holes

listVars :: Typeable a => String -> a -> [Expr]
listVars s a  =  map (`var` a) (variableNamesFromTemplate s)
-- TODO: document and test listVars

listVarsAsTypeOf :: String -> Expr -> [Expr]
listVarsAsTypeOf s e  =  map (`varAsTypeOf` e) (variableNamesFromTemplate s)
-- TODO: document and test listVarsAsTypeOf
