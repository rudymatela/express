-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True

  , evalBool falseE == False
  , evalBool trueE  == True
  , evaluateBool falseE == Just False
  , evaluateBool zero   == Nothing

  , evalInt zero == 0
  , evalInt one  == 1
  , evalInt two   == 2
  , evalInt three == 3
  , evalInt minusOne == -1
  , evaluateInt zero   == Just 0
  , evaluateInt falseE == Nothing

  , show i_ == "_ :: Int"
  , show xx == "x :: Int"
  , show yy == "y :: Int"
  , show zero == "0 :: Int"

  , show xxss == "xs :: [Int]"
  , show yyss == "ys :: [Int]"
  , show nilE == "[] :: [Int]"
  , show (unit one) == "[1] :: [Int]"

  , evalInts nilE == []
  , evalInts (unit one) == [1]

  -- TODO: show "[Char]" as "String"?
  , show emptyStringE == "\"\" :: [Char]"

  , evalString emptyStringE == ""
  ]
