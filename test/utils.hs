-- Copyright (c) 2017-2018 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Haexpress.Utils.List
import Data.Haexpress.Utils.String
import Data.Haexpress.Utils.Typeable

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \xs -> nubSort xs == nub (sort xs :: [Int])
  , holds n $ \xs -> nubSort xs == sort (nub xs :: [Int])

  , holds n $ isSubsetOf ==== (\xs ys -> all (`elem` ys) (xs :: [Int]))

  , elementTy (typeOf [True]) == boolTy
  , elementTy (elementTy (typeOf [[False]])) == boolTy
  , show (mkComparisonTy boolTy) == "Bool -> Bool -> Bool"
  , show (mkComparisonTy intTy)  == "Int -> Int -> Bool"
  , show (mkCompareTy boolTy) == "Bool -> Bool -> Ordering"
  , show (mkCompareTy intTy)  == "Int -> Int -> Ordering"

  , primeCycle [] == []
  , ["x", "y", "z", "x'", "y'", "z'", "x''"] `isPrefixOf` primeCycle ["x","y","z"]
  , ["x","x'","x''","x'''","x''''","x'''''"] `isPrefixOf` primeCycle ["x"]
  , ["i", "j", "k", "i'", "j'", "k'", "i''"] `isPrefixOf` primeCycle ["i","j","k"]
  , ["xy", "zw", "xy'", "zw'", "xy''"]       `isPrefixOf` primeCycle ["xy","zw"]

  , ["x","y","z","x'","y'"]    `isPrefixOf` variableNamesFromTemplate "x"
  , ["xs","ys","zs","xs'"]     `isPrefixOf` variableNamesFromTemplate "xs"
  , ["xss","yss","zss","xss'"] `isPrefixOf` variableNamesFromTemplate "xss"
  , ["c","d","e","c'","d'"]    `isPrefixOf` variableNamesFromTemplate "c"
  , ["s","t","u","s'","t'"]    `isPrefixOf` variableNamesFromTemplate "s"
  , ["0","1","2","3","4"]      `isPrefixOf` variableNamesFromTemplate "0"
  , ["1","2","3","4","5"]      `isPrefixOf` variableNamesFromTemplate "1"
--, ["z","a","b","z'","a'"]    `isPrefixOf` variableNamesFromTemplate "z" -- TODO: make this pass
  , ["x1","x2","x3","x4"]      `isPrefixOf` variableNamesFromTemplate "x1"
  , ["a0","a1","a2","a3"]      `isPrefixOf` variableNamesFromTemplate "a0"
  , ["e1","e2","e3","e4"]      `isPrefixOf` variableNamesFromTemplate "e1"
  , ["xs1","xs2","xs3"]        `isPrefixOf` variableNamesFromTemplate "xs1"
  , ["xy","zw","xy'","zw'"]    `isPrefixOf` variableNamesFromTemplate "xy"
  , ["ab","cd","ab'","cd'"]    `isPrefixOf` variableNamesFromTemplate "ab"
  , ["xys","zws","xys'"]       `isPrefixOf` variableNamesFromTemplate "xys"
  , ["xyz","xyz'","xyz''"]     `isPrefixOf` variableNamesFromTemplate "xyz"
  ]
