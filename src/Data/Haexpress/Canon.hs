-- |
-- Module      : Data.Haexpress.Canon
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Utilities for canonicalizing 'Expr's with variables.
module Data.Haexpress.Canon
  ( canonicalize
  , canonicalizeWith
  , canonicalization
  , canonicalizationWith
  , isCanonical
  , isCanonicalWith
  )
where

import Data.Haexpress.Basic
import Data.Haexpress.Name
import Data.Haexpress.Instances
import Data.List ((\\))

canonicalizeWith :: (Expr -> [String]) -> Expr -> Expr
canonicalizeWith namesFor e = e //- canonicalizationWith namesFor e

canonicalizationWith :: (Expr -> [String]) -> Expr -> [(Expr,Expr)]
canonicalizationWith namesFor e = cr (vars e) []
  where
  cr :: [Expr] -> [(Expr,Expr)] -> [(Expr,Expr)]
  cr []     bs  =  bs
  cr (e:es) bs  =  cr es
                $  if e `elem` map fst bs
                   then bs
                   else (e, (`varAsTypeOf` e) . head $ namesFor e \\ [n | (_,Value ('_':n) _) <- bs]):bs

isCanonicalWith :: (Expr -> [String]) -> Expr -> Bool
isCanonicalWith ti e = canonicalizeWith ti e == e

canonicalize :: Expr -> Expr
canonicalize = canonicalizeWith names'

canonicalization :: Expr -> [(Expr,Expr)]
canonicalization = canonicalizationWith names'

isCanonical :: Expr -> Bool
isCanonical = isCanonicalWith names'

-- 'names' lifted over the 'Expr' type for a handful of prelude Name instances.
names' :: Expr -> [String]
names' e = variableNamesFromTemplate $ case validApps preludeNameInstances e of
  (e':_) -> eval "i" e'
  _      -> "i"
