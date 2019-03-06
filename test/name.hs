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
  ]
