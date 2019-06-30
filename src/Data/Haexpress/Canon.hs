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

-- TODO: use default name instances below instead of defNames

canonicalize :: Expr -> Expr
canonicalize = canonicalizeWith (const $ defNames)

canonicalization :: Expr -> [(Expr,Expr)]
canonicalization = canonicalizationWith (const $ defNames)

isCanonical :: Expr -> Bool
isCanonical = isCanonicalWith (const $ defNames)
