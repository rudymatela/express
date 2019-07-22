-- |
-- Module      : Data.Haexpress
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Expr' type and utilities involving it
{-# LANGUAGE CPP #-}
module Data.Haexpress
  ( 
  -- -- -- Data.Haexpress.Core exports -- -- --

  -- * The Expr datatype
    Expr (..)

  -- * Smart constructors
  , value
  , val
  , ($$)
  , var

  -- * Evaluating Exprs
  , evaluate
  , eval
  , evl
  , typ
  , etyp
  , mtyp
  , toDynamic

  -- * Boolean properties
  , isValue
  , isApp
  , isVar
  , isConst
  , isIllTyped
  , isWellTyped
  , hasVar
  , isGround

  -- * Comparison
  , compareComplexity

  -- * Properties
  , arity
  , size
  , depth
  , height

  -- * Listing subexpressions
  , subexprs
  , values
  , vars
  , consts
  , nubSubexprs
  , nubValues
  , nubVars
  , nubConsts

  -- * Showing Exprs
  , showExpr
  , showPrecExpr
  , showOpExpr

  -- -- -- Data.Haexpress.Map exports -- -- --

  -- * Mapping subexpressions
  , mapValues
  , mapVars
  , mapConsts
  , mapSubexprs
  , (//-)
  , (//)
  , renameVarsBy


  -- -- -- Data.Haexpress.Fold exports -- -- --

  -- * Folding Exprs
  , fold
  , unfold
  , foldPair
  , unfoldPair
  , foldApp
  , unfoldApp


  -- -- -- Data.Haexpress.Hole exports -- -- --

  -- * Creating variables
  , varAsTypeOf
  , listVars
  , listVarsAsTypeOf

  -- * Typed holes
  , hole
  , isHole
  , holes
  , nubHoles
  , holeAsTypeOf

  -- -- -- Data.Haexpress.Canon exports -- -- --

  -- * Canonicalizing Exprs
  , canonicalize
  , canonicalizeWith
  , canonicalization
  , canonicalizationWith
  , isCanonical
  , isCanonicalWith
  , canonicalVariations
  , fastCanonicalVariations

  -- -- -- Data.Haexpress.Match exports -- -- --

  -- * Matching Exprs
  , match
  , matchWith
  , isInstanceOf
  , hasInstanceOf
  , isSubexprOf

  -- -- -- Data.Haexpress.Express exports -- -- --

  -- * The Express typeclass
  , Express (..)
  , deriveExpress
  , deriveExpressCascading
  , deriveExpressIfNeeded

  -- * Other modules
  , module Data.Haexpress.Instances
  , module Data.Haexpress.Name
  , module Data.Haexpress.Name.Derive
  )
where

import Data.Haexpress.Basic
import Data.Haexpress.Canon
import Data.Haexpress.Match
import Data.Haexpress.Instances
import Data.Haexpress.Name
import Data.Haexpress.Name.Derive
import Data.Haexpress.Express
import Data.Haexpress.Express.Derive
