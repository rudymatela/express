-- |
-- Module      : Data.Haexpress.Instances
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines utilities do deal with instances of typeclasses
module Data.Haexpress.Instances
  ( eqFor
  , eqWith
  , compareFor
  , compareWith
  , nameFor
  , nameWith
  , exprFor
  , exprWith
  , validApps
  )
where

import Data.Haexpress.Core
import Data.Haexpress.Name
import Data.Haexpress.Express
import Data.Typeable
import Data.Maybe

eqFor :: (Typeable a, Eq a) => a -> Expr
eqFor a  =  eqWith ((==) -:> a)

eqWith :: Typeable a => (a -> a -> Bool) -> Expr
eqWith (==)  =  value "==" (==)

compareFor :: (Typeable a, Ord a) => a -> Expr
compareFor a  =  compareWith (compare -:> a)

compareWith :: Typeable a => (a -> a -> Ordering) -> Expr
compareWith compare  =  value "compare" compare

nameFor :: (Typeable a, Name a) => a -> Expr
nameFor a  =  nameWith (name -:> a)

nameWith :: Typeable a => (a -> String) -> Expr
nameWith name  =  value "name" name

exprFor :: (Typeable a, Express a) => a -> Expr
exprFor a  =  exprWith (expr -:> a)

exprWith :: Typeable a => (a -> Expr) -> Expr
exprWith expr  =  value "expr" expr

validApps :: [Expr] -> Expr -> [Expr]
validApps es e = mapMaybe ($$ e) es

(-:>) :: (a -> b) -> a -> (a -> b)
(-:>)  =  const
infixl 1 -:>

