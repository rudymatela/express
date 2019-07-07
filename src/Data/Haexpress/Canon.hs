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

canonicalVariations :: Expr -> [Expr]
canonicalVariations = cvars
  where
  cvars e
    | null hs'   =  [e]
    | otherwise  =  concatMap canonicalVariations
                 .  map (fill e) . fillings names
                 $  [h | h <- hs', typ h == typ h']
    where
    hs' = holes e
    h' = head hs'

  names  =  variableNamesFromTemplate "x"

  -- TODO: rename iss
  -- > iss 0 0 = [ [] ]
  -- > iss 0 1 = [ [0] ]
  -- > iss 0 2 = [ [0,1], [0,0] ]
  -- > iss 0 3 = [ [0,1,2], [0,1,0], [0,1,1], [0,0,1], [0,0,0] ]
  iss :: Int -> Int -> [[Int]]
  iss _ 0 = [[]]
  iss i n = concat [map (j:) (iss (i + j-=-i) (n-1)) | j <- i:[0..(i-1)]]
    where x -=- y | x == y    = 1
                  | otherwise = 0

  fillings :: [String] -> [Expr] -> [[Expr]]
  fillings ns hs  =  [ [ns !! f `varAsTypeOf` head hs | f <- fs]
                     | fs <- iss 0 (length hs)]
-- TODO: document canonicalVariations
-- TODO: copy tests from Speculate for canonicalVariations
-- TODO: refactor the above code for clarity
-- TODO: reduce time complexity of canonicalVariations
-- the above is wrong, see:
--
-- > > canonicalVariations (i_ -+- (i_ -+- i_))
-- > [x + (y + z) :: Int,x + (y + y) :: Int,x + (x + y) :: Int,x + (x + x) :: Int]
-- > > vassignments (i_ -+- (i_ -+- i_))
-- > [x + (y + z) :: Int,x + (y + x) :: Int,x + (y + y) :: Int,x + (x + y) :: Int,x + (x + x) :: Int]


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
