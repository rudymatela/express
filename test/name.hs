-- Copyright (c) 2017-2018 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Haexpress.Utils.List

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  -- simple types
  , name (undefined :: Int) == "x"
  , name (undefined :: Integer) == "x"
  , name (undefined :: Char) == "c"
  , name (undefined :: Bool) == "p"
  , name (undefined :: Int -> Int) == "f"
  , name (undefined :: Double) == "x"

  -- lists
  , name (undefined :: [Int]) == "xs"
  , name (undefined :: [[Int]]) == "xss"
  , name (undefined :: [Bool]) == "ps"
  , name (undefined :: [[Bool]]) == "pss"

  -- eithers
  , name (undefined :: Either Bool Bool) == "epq"
  , name (undefined :: Either Int Int) == "exy"
  , name (undefined :: Either Bool Char) == "epc"

  -- maybes
  , name (undefined :: Maybe Int) == "mx"
  , name (undefined :: Maybe [Int]) == "mxs"
  , name (undefined :: Maybe [[Int]]) == "mxss"

  -- pairs
  , name (undefined :: (Int,Int)) == "xy"
  , name (undefined :: (Bool,Bool)) == "pq"
  , name (undefined :: (Char,Char)) == "cd"
  , name (undefined :: (Bool,Int)) == "px"
  , name (undefined :: (Int,Bool)) == "xp"

  -- triples
  , name (undefined :: (Int,Int,Int)) == "xyz"
  , name (undefined :: (Bool,Bool,Bool)) == "pqr"
  , name (undefined :: (Char,Char,Char)) == "cde"
  , name (undefined :: (Int,Bool,Char)) == "xpc"

  -- tuples
  , name (undefined :: (Int,Int,Int,Int)) == "xxxx"
  , name (undefined :: (Int,Int,Int,Int,Int)) == "xxxxx"
  , name (undefined :: (Int,Int,Int,Int,Int,Int)) == "xxxxxx"
  , name (undefined :: (Int,Int,Int,Int,Int,Int,Int)) == "xxxxxxx"
  , name (undefined :: (Int,Int,Int,Int,Int,Int,Int,Int)) == "xxxxxxxx"
  , name (undefined :: (Int,Int,Int,Int,Int,Int,Int,Int,Int)) == "xxxxxxxxx"
  , name (undefined :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)) == "xxxxxxxxxx"
  , name (undefined :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)) == "xxxxxxxxxxx"
  , name (undefined :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int)) == "xxxxxxxxxxxx"

  , name (undefined :: (Bool,Bool,Bool,Bool)) == "pppp"
  , name (undefined :: (Bool,Bool,Bool,Bool,Bool)) == "ppppp"
  , name (undefined :: (Bool,Bool,Bool,Bool,Bool,Bool)) == "pppppp"
  ]
