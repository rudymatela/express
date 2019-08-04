-- |
-- Module      : Data.Express
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Express is a library for manipulating dynamically typed Haskell expressions.
-- It's like "Data.Dynamic" but with support for encoding applications and
-- variables.
--
-- It provides the 'Expr' type and over a hundred functions for
-- building, evaluating, comparing, folding, canonicalizing and matching
-- 'Expr's.
--
-- /Example:/
-- Like with "Data.Dynamic", we can use Express to create heterogeneous lists:
--
-- > > let xs = [val False, val True, val (1::Int), val (2::Int), val (3::Integer), val "123"]
-- > > :t xs
-- > xs :: [Expr]
-- > > xs
-- > [ False :: Bool
-- > , True :: Bool
-- > , 1 :: Int
-- > , 2 :: Int
-- > , 3 :: Integer
-- > , "123" :: [Char]
-- > ]
--
-- We can then apply 'evaluate' to select values of different types:
--
-- > > import Data.Maybe
-- > > mapMaybe evaluate xs :: [Bool]
-- > [False,True]
-- > > mapMaybe evaluate xs :: [Int]
-- > [1,2]
-- > > mapMaybe evaluate xs :: [Integer]
-- > [3]
-- > > mapMaybe evaluate xs :: [String]
-- > ["123"]
--
-- If define an heterogeneous list of functions encoded as 'Expr's:
--
-- > > let fs = [value "not" not, value "&&" (&&), value "abs" (abs :: Int -> Int)]
-- > > :t fs
-- > fs :: [Expr]
--
-- Using '$$' we can list the type correct applications
-- between the two previously defined lists:
--
-- > > catMaybes [f $$ x | f <- fs, x <- xs]
-- > [ not False :: Bool
-- > , not True :: Bool
-- > , (False &&) :: Bool -> Bool
-- > , (True &&) :: Bool -> Bool
-- > , abs 1 :: Int
-- > , abs 2 :: Int
-- > ]
--
-- Other uses of Express include:
--
-- * generalizing counter-examples of property-based testing
--   in <https://hackage.haskell.org/package/extrapolate Extrapolate>;
-- * conjecturing equations based on the results of testing
--   in <https://hackage.haskell.org/package/speculate Speculate>.
--
-- In this documentation,
-- the complexity of most functions is given in big O notation
-- where /n/ is the size of the expression being manipulated or produced.
-- There may still be a /m/ cost associated with the values being stored in 'Expr's.
{-# LANGUAGE CPP #-}
module Data.Express
  ( 
  -- -- -- Data.Express.Core exports -- -- --

  -- * The Expr datatype
    Expr (..)

  -- ** Building Exprs
  , value
  , val
  , ($$)
  , var

  -- ** Evaluating Exprs
  , evaluate
  , eval
  , evl
  , typ
  , etyp
  , mtyp
  , toDynamic

  -- ** Boolean properties of Exprs
  , isValue
  , isApp
  , isVar
  , isConst
  , isIllTyped
  , isWellTyped
  , hasVar
  , isGround

  -- ** Comparing Exprs
  , compareComplexity

  -- ** Properties of Exprs
  , arity
  , size
  , depth
  , height

  -- ** Showing Exprs
  , showExpr
  , showPrecExpr
  , showOpExpr

  -- * Subexpressions

  -- ** Listing subexpressions
  , subexprs
  , values
  , vars
  , consts
  , nubSubexprs
  , nubValues
  , nubVars
  , nubConsts

  -- -- -- Data.Express.Map exports -- -- --

  -- ** Mapping subexpressions
  , mapValues
  , mapVars
  , mapConsts
  , mapSubexprs
  , (//-)
  , (//)
  , renameVarsBy


  -- -- -- Data.Express.Hole exports -- -- --

  -- * Variables and holes

  -- ** Creating variables
  , varAsTypeOf
  , listVars
  , listVarsAsTypeOf

  -- ** Typed holes
  , hole
  , isHole
  , holes
  , nubHoles
  , holeAsTypeOf


  -- -- -- Data.Express.Fold exports -- -- --

  -- * Juggling Exprs

  -- ** Folding Exprs
  , fold
  , unfold
  , foldPair
  , unfoldPair
  , foldTrio
  , unfoldTrio
  , foldApp
  , unfoldApp

  -- -- -- Data.Express.Canon exports -- -- --

  -- ** Canonicalizing Exprs
  , canonicalize
  , canonicalizeWith
  , canonicalization
  , canonicalizationWith
  , isCanonical
  , isCanonicalWith
  , canonicalVariations
  , fastCanonicalVariations

  -- -- -- Data.Express.Match exports -- -- --

  -- ** Matching Exprs
  , match
  , matchWith
  , isInstanceOf
  , hasInstanceOf
  , isSubexprOf

  -- -- -- Data.Express.Express exports -- -- --

  -- * Typeclasses

  -- ** The Express typeclass
  , Express (..)
  , deriveExpress
  , deriveExpressCascading
  , deriveExpressIfNeeded

  -- -- -- Data.Express.Name exports -- -- --

  -- ** The Name typeclass
  , Name (..)
  , names
  , variableNamesFromTemplate
  , deriveName
  , deriveNameCascading
  , deriveNameIfNeeded

  -- -- -- Data.Express.Instances exports -- -- --

  -- ** Typeclass instances as Exprs
  , reifyEq
  , reifyOrd
  , reifyEqOrd
  , reifyName

  , mkEq
  , mkOrd
  , mkOrdLessEqual
  , mkName
  , mkNameWith

  , isEq
  , isOrd
  , isEqOrd
  , isEqT
  , isOrdT
  , isEqOrdT

  , mkEquation
  , mkComparisonLE
  , mkComparisonLT
  , mkComparison
  , lookupComparison

  , listVarsWith
  , lookupName
  , lookupNames

  , validApps
  , findValidApp

  , preludeNameInstances
  )
where

import Data.Express.Basic
import Data.Express.Canon
import Data.Express.Match
import Data.Express.Name
import Data.Express.Name.Derive
import Data.Express.Express
import Data.Express.Express.Derive
import Data.Express.Instances
