-- Copyright (c) 2017-2018 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Haexpress.Utils.List
import Test.LeanCheck.Error (errorToNothing)

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  -- showing expressions

  , show zero == "0 :: Int"
  , show two == "2 :: Int"
  , show minusOne == "-1 :: Int"
  , show (one -+- two -*- three)   == "1 + 2 * 3 :: Int"
  , show ((one -+- two) -*- three) == "(1 + 2) * 3 :: Int"

  , show plusE == "(+) :: Int -> Int -> Int"
  , show timesE == "(*) :: Int -> Int -> Int"

  , show (plusE :$ one) == "(1 +) :: Int -> Int"
  , show (timesE :$ (minusOne -+- two)) == "(((-1) + 2) *) :: Int -> Int"

  -- TODO: make the following work
  -- (It didn't work on Speculate anyway...)
--, show (var "`f`" (undefined :: Int -> Int -> Int) :$ one) == "(1 `f`) :: Int" -- TODO:
--, show (var "`f`" (undefined :: Int -> Int -> Int) :$ one :$ two) == "1 `f` 2 :: Int" -- TODO:
  , show (value "`compare`" (compare :: Int->Int->Ordering) :$ one) == "(1 `compare`) :: Int -> Ordering"
  , show (value "`compare`" (compare :: Int->Int->Ordering) :$ one :$ two) == "1 `compare` 2 :: Ordering"

  , holds n $ show . mapVars (\(Value ('_':s) d) -> Value (if null s then "_" else s) d) === show
--, holds n $ show . mapConsts (\(Value s d) -> Value ('_':s) d) === show -- TODO:

  , show (emptyStringE) == "\"\" :: [Char]"
  , show (spaceE -:- emptyStringE) == "\" \" :: [Char]"
  , show (spaceE -:- ccs)          == "' ':cs :: [Char]"
  , show (ae -:- bee -:- emptyStringE) == "\"ab\" :: [Char]"
  , show (ae -:- bee -:- ccs)          == "'a':'b':cs :: [Char]"
  , show (ae -:- spaceE -:- bee -:- lineBreakE -:- emptyStringE) == "\"a b\\n\" :: [Char]"
  , show (cc -:- spaceE -:- dd -:- lineBreakE -:- emptyStringE) == "c:' ':d:\"\\n\" :: [Char]"
  , show (cc -:- spaceE -:- dd -:- lineBreakE -:- ccs)          == "c:' ':d:'\\n':cs :: [Char]"
  , show (cc -:- ae -:- bee -:- emptyStringE) == "c:\"ab\" :: [Char]"
  , show (cc -:- ae -:- bee -:- spaceE -:- ae -:- bee -:- emptyStringE) == "c:\"ab ab\" :: [Char]"

  , show one                     == "1 :: Int"
  , show (minusOne)              == "-1 :: Int"
  , show (one -+- one)           == "1 + 1 :: Int"
  , show (minusOne -+- minusOne) == "(-1) + (-1) :: Int"

  , show (zero -|- one)          == "(0,1) :: (Int,Int)"
  , show (minusOne -|- minusOne) == "(-1,-1) :: (Int,Int)"
  , show (triple zero one two)   == "(0,1,2) :: (Int,Int,Int)"
  , show (quadruple minusOne zero one two) == "(-1,0,1,2) :: (Int,Int,Int,Int)"
  , show (quintuple minusOne zero one two three) == "(-1,0,1,2,3) :: (Int,Int,Int,Int,Int)"
  , show (sixtuple minusTwo minusOne zero one two three) == "(-2,-1,0,1,2,3) :: (Int,Int,Int,Int,Int,Int)"

  , show (one -:- nilE)                    == "[1] :: [Int]"
  , show (zero -:- one -:- nilE)           == "[0,1] :: [Int]"
  , show (minusOne -:- nilE)               == "[-1] :: [Int]"
  , show (minusOne -:- minusTwo -:- nilE)  == "[-1,-2] :: [Int]"
  , show (xx -:- minusTwo -:- yy -:- nilE) == "[x,-2,y] :: [Int]"
  , show (xx -:- minusTwo -:- yy -:- xxs)  == "x:(-2):y:xs :: [Int]"

  , show (ffE -$- zero)     == "f $ 0 :: Int"
  , show (ggE -$- xx)       == "g $ x :: Int"
  , show (ffE -$- minusOne) == "f $ (-1) :: Int"

  , holds n $ \e -> showExpr e `isPrefixOf` show e
  ]