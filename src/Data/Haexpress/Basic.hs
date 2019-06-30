-- |
-- Module      : Data.Haexpress.Basic
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Expr' type and _basic_ utilities involving it, including:
--
-- * re-export of "Data.Haexpress.Core"
-- * re-export of "Data.Haexpress.Map"
-- * re-export of "Data.Haexpress.Fold"
-- * utilities for creating and manipulating variables and typed holes
--
-- If you're a Haexpress user,
-- you're probably better of importing "Data.Haexpress".
{-# LANGUAGE CPP #-}
module Data.Haexpress.Basic
  (
  -- * Module re-exports
    module Data.Haexpress.Core
  , module Data.Haexpress.Map
  , module Data.Haexpress.Fold

  -- * Creating variables
  , varAsTypeOf

  -- * Typed holes
  , hole
  , isHole
  , holes
  , nubHoles
  )
where

import Data.Haexpress.Core
import Data.Haexpress.Map
import Data.Haexpress.Fold

import Data.Dynamic
import Data.Maybe (fromMaybe)
import Data.Haexpress.Utils.Typeable (tyArity)
import Data.Haexpress.Utils.List (nubSort)

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
-- Creates an 'Expr' representing a typed hole of the given argument type.
--
-- > > hole (undefined :: Int)
-- > _ :: Int
--
-- > > hole (undefined :: Maybe String)
-- > _ :: Maybe [Char]
hole :: Typeable a => a -> Expr
hole a = var "" (undefined `asTypeOf` a)

-- /O(1)/.
isHole :: Expr -> Bool
isHole (Value "_" _)  = True
isHole _              = False
-- TODO: document and test isHole
-- TODO: document isHole ==> isVar

holes :: Expr -> [Expr]
holes  =  filter isHole . values
-- TODO: document and test holes
-- TODO: property  holes `isSubsequenceOf` vars

nubHoles :: Expr -> [Expr]
nubHoles  =  nubSort . holes
-- TODO: document and test nubHoles
-- TODO: property nubHoles `isSubsetOf` holes
