-- |
-- Module      : Data.Express.Canon
-- Copyright   : (c) 2019-2020 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Utilities for canonicalizing 'Expr's with variables.
module Data.Express.Canon
  ( canonicalize
  , canonicalizeWith
  , canonicalization
  , canonicalizationWith
  , isCanonical
  , isCanonicalWith
  , canonicalVariations
  , fastCanonicalVariations
  )
where

import Data.Express.Basic
import Data.Express.Name
import Data.Express.Instances
import Data.List ((\\))

-- |
-- Like 'canonicalize' but allows customization
-- of the list of variable names.
-- (cf. 'lookupNames', 'variableNamesFromTemplate')
--
-- > > canonicalizeWith (const ["i","j","k","l",...]) (xx -+- yy)
-- > i + j :: Int
--
-- The argument 'Expr' of the argument function allows
-- to provide a different list of names for different types:
--
-- > > let namesFor e
-- > >   | typ e == typeOf (undefined::Char) = variableNamesFromTemplate "c1"
-- > >   | typ e == typeOf (undefined::Int)  = variableNamesFromTemplate "i"
-- > >   | otherwise                         = variableNamesFromTemplate "x"
--
-- > > canonicalizeWith namesFor ((xx -+- ord' dd) -+- (ord' cc -+- yy))
-- > (i + ord c1) + (ord c2 + j) :: Int
canonicalizeWith :: (Expr -> [String]) -> Expr -> Expr
canonicalizeWith namesFor e = e //- canonicalizationWith namesFor e

-- |
-- Like 'canonicalization' but allows customization
-- of the list of variable names.
-- (cf. 'lookupNames', 'variableNamesFromTemplate')
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

-- |
-- Like 'isCanonical' but allows specifying
-- the list of variable names.
isCanonicalWith :: (Expr -> [String]) -> Expr -> Bool
isCanonicalWith ti e = canonicalizeWith ti e == e

-- |
-- Canonicalizes an 'Expr' so that variable names appear in order.
-- Variable names are taken from the 'preludeNameInstances'.
--
-- > > canonicalize (xx -+- yy)
-- > x + y :: Int
--
-- > > canonicalize (yy -+- xx)
-- > x + y :: Int
--
-- > > canonicalize (xx -+- xx)
-- > x + x :: Int
--
-- > > canonicalize (yy -+- yy)
-- > x + x :: Int
--
-- Constants are untouched:
--
-- > > canonicalize (jj -+- (zero -+- abs' ii))
-- > x + (y + abs y) :: Int
--
-- This also works for variable functions:
--
-- > > canonicalize (gg yy -+- ff xx -+- gg xx)
-- > (f x + g y) + f y :: Int
canonicalize :: Expr -> Expr
canonicalize = canonicalizeWith names'

-- |
-- Return a canonicalization of an 'Expr'
-- that makes variable names appear in order
-- using 'names' as provided by 'preludeNameInstances'.
-- By using '//-' it can 'canonicalize' 'Expr's.
--
-- > > canonicalization (gg yy -+- ff xx -+- gg xx)
-- > [ (x :: Int,        y :: Int)
-- > , (f :: Int -> Int, g :: Int -> Int)
-- > , (y :: Int,        x :: Int)
-- > , (g :: Int -> Int, f :: Int -> Int) ]
--
-- > > canonicalization (yy -+- xx -+- yy)
-- > [ (x :: Int, y :: Int)
-- > , (y :: Int, x :: Int) ]
canonicalization :: Expr -> [(Expr,Expr)]
canonicalization = canonicalizationWith names'

-- |
-- Returns whether an 'Expr' is canonical:
-- if applying 'canonicalize' is an identity
-- using 'names' as provided by 'preludeNameInstances'.
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
-- > [ x + y :: Int
-- > , x + x :: Int ]
--
-- > > canonicalVariations $ i_ -+- i_ -+- i_
-- > [ (x + y) + z :: Int
-- > , (x + y) + x :: Int
-- > , (x + y) + y :: Int
-- > , (x + x) + y :: Int
-- > , (x + x) + x :: Int ]
--
-- > > canonicalVariations $ i_ -+- ord' c_
-- > [x + ord c :: Int]
--
-- > > canonicalVariations $ i_ -+- i_ -+- ord' c_
-- > [ (x + y) + ord c :: Int
-- > , (x + x) + ord c :: Int ]
--
-- > > canonicalVariations $ i_ -+- i_ -+- length' (c_ -:- unit c_)
-- > [ (x + y) + length (c:d:"") :: Int
-- > , (x + y) + length (c:c:"") :: Int
-- > , (x + x) + length (c:d:"") :: Int
-- > , (x + x) + length (c:c:"") :: Int ]
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
--
-- Behaviour is undefined when applying this function to expressions already
-- containing variables.
canonicalVariations :: Expr -> [Expr]
canonicalVariations = map canonicalize . fastCanonicalVariations

-- |
-- A faster version of 'canonicalVariations' that
-- disregards name clashes across different types.
-- Results are confusing to the user
-- but fine for Express which differentiates
-- between variables with the same name but different types.
--
-- Without applying 'canonicalize', the following 'Expr'
-- may seem to have only one variable:
--
-- > > fastCanonicalVariations $ i_ -+- ord' c_
-- > [x + ord x :: Int]
--
-- Where in fact it has two, as the second @ x @ has a different type.
-- Applying 'canonicalize' disambiguates:
--
-- > > map canonicalize . fastCanonicalVariations $ i_ -+- ord' c_
-- > [x + ord c :: Int]
--
-- This function is useful when resulting 'Expr's are
-- not intended to be presented to the user
-- but instead to be used by another function.
-- It is simply faster to skip the step where clashes are resolved.
fastCanonicalVariations :: Expr -> [Expr]
fastCanonicalVariations e
  | null hs'   =  [e]
  | otherwise  =  concatMap fastCanonicalVariations
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
