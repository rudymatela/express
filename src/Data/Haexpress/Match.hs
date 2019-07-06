-- |
-- Module      : Data.Haexpress.Match
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Utilities for matching 'Expr's with 'var'iables.
module Data.Haexpress.Match
  ( match
  , matchWith
  , isInstanceOf
  , hasInstanceOf
  )
where

import Data.Haexpress.Basic
import Data.Maybe
import Data.Functor ((<$>))
import Control.Monad ((>=>))

-- | List matches if possible
--
-- > 0 + 1       `match` x + y       = Just [x=0, y=1]
-- > 0 + (1 + 2) `match` x + y       = Just [x=0, y=1 + 2]
-- > 0 + (1 + 2) `match` x + (y + y) = Nothing
-- > (x + x) + (1 + 2) `match` x + (y + y) = Nothing
match :: Expr -> Expr -> Maybe [(Expr,Expr)]
match = matchWith []
-- TODO: proper documentation for match

-- | List matches with preexisting bindings:
--
-- > 0 + 1 `matchWith [(x,0)]` x + y = Just [x=0, y=1]
-- > 0 + 1 `matchWith [(x,1)]` x + y = Nothing
matchWith :: [(Expr,Expr)] -> Expr -> Expr -> Maybe [(Expr,Expr)]
matchWith bs e1' e2' = m e1' e2' bs
  where
  m :: Expr -> Expr -> [(Expr,Expr)] -> Maybe [(Expr,Expr)]
  m (f1 :$ x1) (f2 :$ x2)             =  m f1 f2 >=> m x1 x2
  m e1 e2
    | isVar e2 && mtyp e1 == mtyp e2  =  updateAssignments (e2,e1)
    | e1 == e2                        =  Just
    | otherwise                       =  const Nothing
-- TODO: proper documentation for matchWith

updateAssignments :: (Expr,Expr) -> [(Expr,Expr)] -> Maybe [(Expr,Expr)]
updateAssignments (e,e') = \bs ->
  case lookup e bs of
    Nothing  -> Just ((e,e'):bs)
    Just e'' -> if e'' == e'
                then Just bs
                else Nothing

-- 0 `isInstanceOf` x = True
-- y `isInstanceOf` x = True
-- x `isInstanceOf` 0 = False
-- 1 `isInstanceOf` 0 = False
-- x + (y + x) `isInstanceOf` x + y = True
-- y + (y + x) `isInstanceOf` x + y = True
-- 0 + (y + x) `isInstanceOf` x + y = True
-- x `isInstanceOf` x = True
-- _ `isInstanceOf` x = True
isInstanceOf :: Expr -> Expr -> Bool
e1 `isInstanceOf` e2 = isJust $ e1 `match` e2

hasInstanceOf :: Expr -> Expr -> Bool
e1           `hasInstanceOf` e2 | e1   `isInstanceOf` e2 = True
(e1f :$ e1x) `hasInstanceOf` e2 | e1f `hasInstanceOf` e2 ||
                                  e1x `hasInstanceOf` e2 = True
_            `hasInstanceOf` _                           = False
