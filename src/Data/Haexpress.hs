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
  , (\\\)
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

type Substitution = [(String,Expr)]

findSub :: String -> Dynamic -> Substitution -> Maybe Expr
findSub n d bs = snd <$> find (\(n',e) -> n' == n && typ e == dynTypeRep d) bs


-- | /O(n)/.
-- Substitute all occurrences of a variable in an expression.
--
-- > > ((xx -+- yy) -+- (yy -+- zz)) \\\ [("y", yy -+- zz)] =
-- > (x + (y + z)) + ((y + z) + z)
--
-- Note this is /not/ equivalent to @foldr sub1@.  Variables inside
-- expressions being assigned will not be assigned.
(\\\) :: Expr -> Substitution -> Expr
(e1 :$ e2)          \\\ as  =  (e1 \\\ as) :$ (e2 \\\ as)
e@(Value ('_':n) d) \\\ as  =  fromMaybe e $ findSub n d as
e                   \\\ as  =  e

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
