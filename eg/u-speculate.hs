-- u-speculate.hs -- micro Speculate / Speculite
--
-- Copyright (c) 2019-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- A small library capable of conjecturing laws about Haskell functions
-- implemented in under 70 lines of code.
--
-- This works like the equation conjecturing tools Speculate or QuickSpec,
-- combining expressions to form equations,
-- reporting any equations that passes 60 tests.
-- Redundant equations are then pruned using simple rules.
--
-- Limitations:
--
-- * this program prints redundant equations;
-- * there is no way to configure maximum number of tests to consider an
--   equation true;
-- * runtime is exponential as you add more symbols to speculate about.
--
-- Please see Speculate for a full featured version:
--
--   https://github.com/rudymatela/speculate
import Data.List
import Data.Maybe
import Data.Express
import Test.LeanCheck

main :: IO ()
main  =  do
  printEquationsAbout
    [ hole (undefined :: Int)
    , val (0 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "abs" (abs :: Int -> Int)
    ]

  printEquationsAbout
    [ hole (undefined :: Bool)
    , val False
    , val True
    , value "not" not
    ]

  printEquationsAbout
    [ hole (undefined :: Int)
    , hole (undefined :: [Int])
    , val ([] :: [Int])
    , value ":" ((:) :: Int -> [Int] -> [Int])
    , value "++" ((++) :: [Int] -> [Int] -> [Int])
    , value "sort" (sort :: [Int] -> [Int])
    ]

  {-
  printEquationsAbout
    [ hole (undefined :: Bool)
    , hole (undefined :: [Bool])
    , val True
    , val ([] :: [Bool])
    , value "foldr" (foldr :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool)
    , value "&&" (&&)
    , value "and" (and :: [Bool] -> Bool)
    ]
  -}

printEquationsAbout :: [Expr] -> IO ()
printEquationsAbout es  =  do
  putStrLn $ "Equations about " ++ intercalate ", " (map showExpr es) ++ ":"
  putStrLn . unlines . map showEq $ speculateAbout es
  where
  showEq eq  =  showExpr (lhs eq) ++ "  =  " ++ showExpr (rhs eq)

speculateAbout :: [Expr] -> [Expr]
speculateAbout  =  discardLater canBeSimplifiedBy
                .  discardLater isInstanceOf
                .  concatMap trueCanonicalVariations
                .  discardLater (\e1 e2 -> isntIdentity e2 && e2 `isInstanceOf` e1)
                .  sort
                .  filter isTrue
                .  candidateEquationsFrom
  where
  e1 `canBeSimplifiedBy` e2  =  isRule e2 && e1 `hasInstanceOf` lhs e2

trueCanonicalVariations :: Expr -> [Expr]
trueCanonicalVariations  =  discardLater isInstanceOf
                         .  filter isTrue
                         .  filter isntIdentity
                         .  canonicalVariations

candidateEquationsFrom :: [Expr] -> [Expr]
candidateEquationsFrom es'  =  [e1 -==- e2 | e1 <- es, e2 <- es, e1 >= e2]
  where
  es  =  candidateExprsFrom es'

candidateExprsFrom :: [Expr] -> [Expr]
candidateExprsFrom  =  concat . take 5 . expressionsT
  where
  expressionsT ds  =  [ds] \/ (delay $ productMaybeWith ($$) es es)
    where
    es  =  expressionsT ds

isTrue :: Expr -> Bool
isTrue  =  all (eval False) . take 60 . grounds

grounds :: Expr -> [Expr]
grounds e  =  map (e //-) . concat $ products [mapT ((,) v) (tiersFor v) | v <- nubVars e]

tiersFor :: Expr -> [[Expr]]
tiersFor e  =  case show (typ e) of
  "Int"    ->  mapT val (tiers :: [[Int]])
  "Bool"   ->  mapT val (tiers :: [[Bool]])
  "[Int]"  ->  mapT val (tiers :: [[ [Int] ]])
  "[Bool]" ->  mapT val (tiers :: [[ [Bool] ]])
  _        ->  []

(-==-) :: Expr -> Expr -> Expr
ex -==- ey  =  head $
  [eqn | eq <- eqs, let eqn = eq :$ ex :$ ey, isWellTyped eqn] ++ [val False]
  where
  eqs  =  [ value "==" ((==) :: Int -> Int -> Bool)
          , value "==" ((==) :: Bool -> Bool -> Bool)
          , value "==" ((==) :: [Int] -> [Int] -> Bool)
          , value "==" ((==) :: [Bool] -> [Bool] -> Bool)
          ]

lhs, rhs :: Expr -> Expr
lhs (((Value "==" _) :$ e) :$ _)  =  e
rhs (((Value "==" _) :$ _) :$ e)  =  e

isntIdentity, isRule :: Expr -> Bool
isntIdentity eq  =  lhs eq /= rhs eq
isRule       eq  =  size (lhs eq) > size (rhs eq)

discardLater :: (a -> a -> Bool) -> [a] -> [a]
discardLater (?)  =  d
  where
  d []      =  []
  d (x:xs)  =  x : d (discard (? x) xs)
  discard p  =  filter (not . p)
