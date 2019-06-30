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
    module Data.Haexpress.Basic
  , module Data.Haexpress.Instances
  , module Data.Haexpress.Name
  , module Data.Haexpress.Express
  , isSubexpr
  )
where

import Data.Haexpress.Basic
import Data.Haexpress.Instances
import Data.Haexpress.Name
import Data.Haexpress.Express

-- TODO: move stuff below into submodules of its own:

-- | /O(n)/?
isSubExprOf :: Expr -> Expr -> Bool
e `isSubExprOf` e0 | e == e0  =  True
e `isSubExprOf` (e1 :$ e2)    =  e `isSubExprOf` e1
                              || e `isSubExprOf` e2
e `isSubExprOf` _             =  False

isSubexpr :: Expr -> Expr -> Bool
isSubexpr e = (e `elem`) . subexprs
-- TODO: document & test isSubexpr
-- TODO: aren't isSubExprOf and isSubexpr the same thing?  test...
