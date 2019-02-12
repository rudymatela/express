-- |
-- Module      : Data.Haexpress
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Expr' type and utilities involving it
module Data.Haexpress
  ( module Data.Haexpress.Core -- TODO: explicitly re-export core entities
  , module Data.Haexpress.Name
  , (\\\)
  )
where

import Data.Haexpress.Core
import Data.Haexpress.Name


-- TODO: move stuff below into submodules of its own:

import Data.Dynamic
import Data.List (find)
import Data.Maybe (fromMaybe)

type Substitution = [(String,Expr)]

findSub :: String -> Dynamic -> Substitution -> Maybe Expr
findSub n d bs = snd <$> find (\(n',e) -> n' == n && typ e == dynTypeRep d) bs


-- | Substitute all occurrences of a variable in an expression.
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

-- TODO: sub1
