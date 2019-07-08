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
  , canonicalVariations
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
                   else (e, n `varAsTypeOf` e):bs
    where
    existingNames = [n | (_,Value ('_':n) _) <- bs]
    freshNames = namesFor e \\ existingNames
    n = head freshNames

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
names' = lookupNames preludeNameInstances

-- |
-- Returns all canonical variations of an 'Expr'
-- by filling holes with variables.
-- Where possible, variations are listed
-- from most general to least general.
--
-- > > canonicalVariations $ i_
-- > [x :: Int]
--
-- > > canonicalVariations $ i_ -+- i_
-- > [x + y :: Int,x + x :: Int]
--
-- > > canonicalVariations $ i_ -+- i_ -+- i_
-- > [(x + y) + z :: Int,(x + y) + x :: Int,(x + y) + y :: Int,(x + x) + y :: Int,(x + x) + x :: Int]
--
-- When presenting resulting 'Expr's to the user,
-- it is recommented to 'canonicalize' first.
-- This function leverages that 'Haexpress' differentiates
-- between variables with the same name but different types.
-- Without applying 'canonicalize', the following 'Expr'
-- may seem to have only one variable:
--
-- > canonicalVariations $ i_ -+- ord' c_
-- [x + ord x :: Int]
--
-- Where in fact it has two:
--
-- > map canonicalize . canonicalVariations $ i_ -+- ord' c_
-- [x + ord c :: Int]
--
-- We could always 'canonicalize' after running this function
-- but when not presenting resulting 'Expr's to the user
-- it is simply more efficient to not do it.
--
-- In an expression without holes this functions just returns a singleton list
-- with the expression itself:
--
-- > > canonicalVariations $ val (0 :: Int)
-- > [0 :: Int]
--
-- > > canonicalVariations $ ord' bee
-- > [ord 'b' :: Int]
--
-- > > canonicalVariations $ ii -+- jj
-- > [i + j :: Int]
canonicalVariations :: Expr -> [Expr]
canonicalVariations = cvars
  where
  cvars e
    | null hs'   =  [e]
    | otherwise  =  concatMap canonicalVariations
                 .  map (fill e) . fillings 0
                 $  [h | h <- hs', typ h == typ h']
    where
    hs' = holes e
    h' = head hs'

  names  =  variableNamesFromTemplate "x"

  fillings :: Int -> [Expr] -> [[Expr]]
  fillings i []  =  [[]] -- no holes, single empty filling
  fillings i (h:hs)  =
    concat $ map (names !! i `varAsTypeOf` h:) (fillings (i+1) hs) -- new var
           : [ map (n `varAsTypeOf` h:) (fillings i hs) -- no new variable
             | n <- take i names ]
-- TODO: document canonicalVariations
-- TODO: copy tests from Speculate for canonicalVariations


-- | Fill holes in an expression.
--   Silently skips holes that are not of the right type.
--   Silently discard remaining expressions.
fill :: Expr -> [Expr] -> Expr
fill e = fst . fill' e
  where
  fill' :: Expr -> [Expr] -> (Expr,[Expr])
  fill' (e1 :$ e2) es = let (e1',es')  = fill' e1 es
                            (e2',es'') = fill' e2 es'
                        in (e1' :$ e2', es'')
  fill' eh (e:es) | isHole eh && typ eh == typ e = (e,es)
  fill' e es = (e,es)
-- TODO: copy tests from Speculate
-- TODO: add examples on Haddock
-- TODO: export? consider exporting 'fill' from the Hole module
