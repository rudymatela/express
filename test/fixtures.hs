-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True
  
  , show i_ == "_ :: Int" -- TODO: fix me
  , show xx == "x :: Int"
  , show yy == "y :: Int"
  ]
