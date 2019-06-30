-- Copyright (c) 2017-2018 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Haexpress.Utils.List

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  -- TODO: test tuples
  -- TODO: split the following into setions
  , name (undefined :: Int) == "x"
  , name (undefined :: Integer) == "x"
  , name (undefined :: Char) == "c"
  , name (undefined :: Bool) == "p"
  , name (undefined :: [Int]) == "xs"
  , name (undefined :: [[Int]]) == "xss"
  , name (undefined :: [Bool]) == "ps"
  , name (undefined :: [[Bool]]) == "pss"
  , name (undefined :: Either Bool Char) == "epc"
  , name (undefined :: Maybe Int) == "mx"
  , name (undefined :: Maybe [Int]) == "mxs"
  , name (undefined :: Maybe [[Int]]) == "mxss"

  , ["x","y","z","x'","y'"]    `isPrefixOf` namesFromTemplate "x"
  , ["xs","ys","zs","xs'"]     `isPrefixOf` namesFromTemplate "xs"
  , ["xss","yss","zss","xss'"] `isPrefixOf` namesFromTemplate "xss"
  , ["c","d","e","c'","d'"]    `isPrefixOf` namesFromTemplate "c"
  , ["s","t","u","s'","t'"]    `isPrefixOf` namesFromTemplate "s"
  , ["0","1","2","3","4"]      `isPrefixOf` namesFromTemplate "0"
  , ["1","2","3","4","5"]      `isPrefixOf` namesFromTemplate "1"
--, ["z","a","b","z'","a'"]    `isPrefixOf` namesFromTemplate "z" -- TODO: make this pass
  , ["x1","x2","x3","x4"]      `isPrefixOf` namesFromTemplate "x1"
  , ["a0","a1","a2","a3"]      `isPrefixOf` namesFromTemplate "a0"
  , ["e1","e2","e3","e4"]      `isPrefixOf` namesFromTemplate "e1"
  , ["xs1","xs2","xs3"]        `isPrefixOf` namesFromTemplate "xs1"
  , ["xy","zw","xy'","zw'"]    `isPrefixOf` namesFromTemplate "xy"
  , ["ab","cd","ab'","cd'"]    `isPrefixOf` namesFromTemplate "ab"
  , ["xys","zws","xys'"]       `isPrefixOf` namesFromTemplate "xys"
  , ["xyz","xyz'","xyz''"]     `isPrefixOf` namesFromTemplate "xyz"
  ]
