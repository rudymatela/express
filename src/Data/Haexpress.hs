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

  -- -- -- Data.Haexpress.Map exports -- -- --

  -- ** Mapping subexpressions
  , mapValues
  , mapVars
  , mapConsts
  , mapSubexprs
  , (//-)
  , (//)
  , renameVarsBy


  -- -- -- Data.Haexpress.Hole exports -- -- --

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


  -- -- -- Data.Haexpress.Fold exports -- -- --

  -- * Juggling Exprs

  -- ** Folding Exprs
  , fold
  , unfold
  , foldPair
  , unfoldPair
  , foldApp
  , unfoldApp

  -- -- -- Data.Haexpress.Canon exports -- -- --

  -- ** Canonicalizing Exprs
  , canonicalize
  , canonicalizeWith
  , canonicalization
  , canonicalizationWith
  , isCanonical
  , isCanonicalWith
  , canonicalVariations
  , fastCanonicalVariations

  -- -- -- Data.Haexpress.Match exports -- -- --

  -- ** Matching Exprs
  , match
  , matchWith
  , isInstanceOf
  , hasInstanceOf
  , isSubexprOf

  -- -- -- Data.Haexpress.Express exports -- -- --

  -- * Typeclasses

  -- ** The Express typeclass
  , Express (..)
  , deriveExpress
  , deriveExpressCascading
  , deriveExpressIfNeeded

  -- -- -- Data.Haexpress.Name exports -- -- --

  -- ** The Name typeclass
  , Name (..)
  , names
  , variableNamesFromTemplate
  , deriveName
  , deriveNameCascading
  , deriveNameIfNeeded

  -- -- -- Data.Haexpress.Instances exports -- -- --

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

import Data.Haexpress.Basic
import Data.Haexpress.Canon
import Data.Haexpress.Match
import Data.Haexpress.Name
import Data.Haexpress.Name.Derive
import Data.Haexpress.Express
import Data.Haexpress.Express.Derive
import Data.Haexpress.Instances
