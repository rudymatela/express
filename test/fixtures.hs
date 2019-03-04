-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True

  -- Bool --

  , show b_ == "_ :: Bool"
  , show pp == "p :: Bool"
  , show qq == "q :: Bool"
  , show false == "False :: Bool"
  , show true == "True :: Bool"
  , show notE == "not :: Bool -> Bool"
  , show andE == "(&&) :: Bool -> Bool -> Bool"
  , show orE  == "(||) :: Bool -> Bool -> Bool"
  , show (not' false) == "not False :: Bool"
  , show (false -&&- true) == "False && True :: Bool"
  , show (pp -||- false) == "p || False :: Bool"
  , show (qq -&&- true) == "q && True :: Bool"

  , evl false == False
  , evl true  == True
  , holds n $ evl notE === not
  , holds n $ evl andE ==== (&&)
  , holds n $ evl orE  ==== (||)
  , evl (not' false) == True
  , evl (false -&&- true) == False
  , evl (false -||- true) == True
  , holds n $ \p -> evl (not' (val p)) == not p
  , holds n $ \p q -> evl (val p -&&- val q) == (p && q)
  , holds n $ \p q -> evl (val p -||- val q) == (p || q)

  -- Int --

  , show i_ == "_ :: Int"
  , show xx == "x :: Int"
  , show yy == "y :: Int"
  , show zz == "z :: Int"
  , show xx' == "x' :: Int"
  , show zero == "0 :: Int"
  , show two == "2 :: Int"
  , show minusOne == "-1 :: Int"

  , evl zero == (0 :: Int)
  , evl one  == (1 :: Int)
  , evl two   == (2 :: Int)
  , evl three == (3 :: Int)
  , evl minusOne == (-1 :: Int)

  , show xxs  == "xs :: [Int]"
  , show yys  == "ys :: [Int]"
  , show nilE == "[] :: [Int]"
  , show (unit one) == "[1] :: [Int]"

  -- Int -> Int --

  , show ffE == "f :: Int -> Int"
  , show ggE == "g :: Int -> Int"

  , evalInts nilE == []
  , evalInts (unit one) == [1]

  -- TODO: show "[Char]" as "String"?
  , show emptyStringE == "\"\" :: [Char]"

  , evalString emptyStringE == ""


  -- evaluate --

  , evaluateBool false == Just False
  , evaluateBool zero   == Nothing

  , evaluateInt zero   == Just 0
  , evaluateInt false == Nothing

  , evalChar bee == 'b'
  , evaluateChar cee  == Just 'c'
  , evaluateChar zero == Nothing

  , evalInts (unit one) == [1]
  , evaluateInts (zero -:- unit one) == Just [0,1]
  , evaluateInts (bee -:- cee -:- unit dee) == Nothing

  , evalString (bee -:- cee -:- unit dee) == "bcd"
  , evaluateString (zero -:- unit one) == Nothing
  , evaluateString (bee -:- cee -:- unit dee) == Just "bcd"


  ]
