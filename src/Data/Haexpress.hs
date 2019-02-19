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
-- TODO: explicitly export everything instead of the modules
    module Data.Haexpress.Core
  , module Data.Haexpress.Name
  , module Data.Haexpress.Express
  , (//)
  , varAsTypeOf
  )
where

import Data.Haexpress.Core
import Data.Haexpress.Name
import Data.Haexpress.Express


-- TODO: move stuff below into submodules of its own:

import Data.Dynamic
import Data.List (find)
import Data.Maybe (fromMaybe)

-- TODO: implement mapValues
-- TODO: implement mapVars and use it on //-
-- TODO: implement mapConsts
-- TODO: implement mapOuter
-- TODO: implement mapMaybeOuter :: (Expr -> Maybe Expr) -> Expr -> Expr
--                 and use it on //, maybe ...
-- TODO: implement mapInner


-- | /O(n+m*v)/.
-- Substitute all occurrences of variables in an expression.
--
-- > > ((xx -+- yy) -+- (yy -+- zz)) // [(yy, yy -+- zz)] =
-- > (x + (y + z)) + ((y + z) + z)
(//-) :: Expr -> [(Expr,Expr)] -> Expr
(e1 :$ e2)          //- s  =  (e1 //- s) :$ (e2 //- s)
e@(Value ('_':_) _) //- s  =  e // s
e                   //- s  =  e

-- | /O(n+n*m)/.
-- Substitute subexpressions in an expression.
-- Larger expressions take more precedence.  <-- TODO: explain this
(//) :: Expr -> [(Expr,Expr)] -> Expr
e // s  =  fromMaybe r $ snd <$> find ((== e) . fst) s
  where
  r = case e of
      (e1 :$ e2) -> (e1 // s) :$ (e2 // s)
      e          -> e

-- implement \\\\ which works for all types of subterms, not only terminal ones

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
