-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True
  
  , evalInt zero  == 0
  , evalInt one   == 1
  , evalInt two   == 2
  , evalInt three == 3
  , evalInt minusOne == -1
  , show i_ == "_ :: Int"
  , show xx == "x :: Int"
  , show yy == "y :: Int"
  , show zero == "0 :: Int"
  ]
