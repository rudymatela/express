-- u-extrapolate.hs -- micro Extrapolate / Extrapolite
--
-- Copyright (c) 2019-2021 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- A small property-based testing library capable of generalizing
-- counterexamples implemented in under 50 lines of code.
--
-- This example works like other property-based testing libraries
-- like QuickCheck, LeanCheck or SmallCheck:
--
-- When given a property, it will test it for 500 test arguments.
-- If no counterexample is found, "tests passed" is reported.
-- If a counterexample is found, it is reported.
--
-- However, when a counterexample is found, this program will try to generalize
-- it by replacing subexpressions to variables.  If a generalization that
-- _fails_ 500 tests is found, it is reported.
--
-- Limitations:
--
-- * this only supports properties with one argument (uncurried).
-- * this only supports generalization of Int, Bool, [Int] and [Bool] values.
-- * there is no way to configure the number of test arguments.
--
-- Please see Extrapolate for a full-featured version:
--
--   https://github.com/rudymatela/extrapolate
import Data.List
import Data.Maybe
import Data.Express
import Test.LeanCheck hiding (counterExamples, check)

main :: IO ()
main  =  do
  putStrLn "sort . sort = sort"
  check $ \xs -> sort (sort xs :: [Int]) == sort xs

  putStrLn "length . nub = length  (incorrect when there are repeated elements)"
  check $ \xs -> length (nub xs :: [Int]) == length xs

  putStrLn "xs `union` ys == ys `union` xs"
  check $ \(xs,ys) -> xs `union` ys == ys `union` (xs :: [Int])

  putStrLn "\\(x,y) -> x + y == y + x"
  check $ \(x,y) -> x + y == y + (x :: Int)

  putStrLn "\\x -> x == x + 1  (always incorrect)"
  check $ \x -> x == x + (1 :: Int)

  putStrLn "\\(x,y) -> x + y == x + x  (incorrect)"
  check $ \(x,y) -> x + y == x + (x :: Int)

  putStrLn "\\(x,y) -> x /= y  (incorrect whenever x and y are equal)"
  check $ \(x,y) -> x /= (y :: Int)


check :: (Listable a, Express a) => (a -> Bool) -> IO ()
check prop  =  putStrLn $ case counterExamples 500 prop of
  []     -> "+++ Tests passed.\n"
  (ce:_) -> unlines $ ("*** Falsified, counterexample:  " ++ showExpr ce)
                    : ["               generalization:  " ++ showExpr g
                      | g <- counterExampleGeneralizations 500 prop ce ]

counterExamples :: (Listable a, Express a) => Int -> (a -> Bool) -> [Expr]
counterExamples maxTests prop  =  [expr x | x <- take maxTests list, not (prop x)]

counterExampleGeneralizations :: Express a => Int -> (a -> Bool) -> Expr -> [Expr]
counterExampleGeneralizations maxTests prop e  =  discardLaterThat isInstanceOf
  [ g | g <- candidateGeneralizations e
      , all (not . prop . evl) (take maxTests $ grounds g) ]


candidateGeneralizations :: Expr -> [Expr]
candidateGeneralizations  =  map canonicalize
                          .  concatMap canonicalVariations
                          .  gen
  where
  gen e@(e1 :$ e2)  =
    [holeAsTypeOf e | isListable e]
    ++ [g1 :$ g2 | g1 <- gen e1, g2 <- gen e2]
    ++ map (:$ e2) (gen e1)
    ++ map (e1 :$) (gen e2)
  gen e
    | isVar e    =  []
    | otherwise  =  [holeAsTypeOf e | isListable e]
  isListable  =  not . null . tiersFor

grounds :: Expr -> [Expr]
grounds e  =  map (e //-)
           .  concat
           $  products [mapT ((,) v) (tiersFor v) | v <- nubVars e]

tiersFor :: Expr -> [[Expr]]
tiersFor e  =  case show (typ e) of
  "Int"    ->  mapT val (tiers `asTypeOf` [[undefined :: Int]])
  "Bool"   ->  mapT val (tiers `asTypeOf` [[undefined :: Bool]])
  "[Int]"  ->  mapT val (tiers `asTypeOf` [[undefined :: [Int]]])
  "[Bool]" ->  mapT val (tiers `asTypeOf` [[undefined :: [Bool]]])
  _        ->  []

discardLaterThat :: (a -> a -> Bool) -> [a] -> [a]
discardLaterThat (?)  =  d
  where
  d []      =  []
  d (x:xs)  =  x : d (discard (? x) xs)
  discard p  =  filter (not . p)
