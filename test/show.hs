-- Copyright (c) 2017-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Express.Utils.List

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  -- showing expressions

  , show zero == "0 :: Int"
  , show two == "2 :: Int"
  , show minusOne == "-1 :: Int"
  , show (one -+- two -*- three)   == "1 + 2 * 3 :: Int"
  , show ((one -+- two) -*- three) == "(1 + 2) * 3 :: Int"

  , show plus == "(+) :: Int -> Int -> Int"
  , show times == "(*) :: Int -> Int -> Int"

  , show (plus :$ one) == "(1 +) :: Int -> Int"
  , show (times :$ (minusOne -+- two)) == "(((-1) + 2) *) :: Int -> Int"

  , show ffE == "f :: Int -> Int"
  , show (ff xx) == "f x :: Int"
  , show (var "f" (undefined :: Int -> Int -> Int)) == "f :: Int -> Int -> Int"
  , show (var "f" (undefined :: Int -> Int -> Int) :$ one) == "f 1 :: Int -> Int"
  , show (var "f" (undefined :: Int -> Int -> Int) :$ one :$ two) == "f 1 2 :: Int"
  , show (var "`f`" (undefined :: Int -> Int -> Int)) == "f :: Int -> Int -> Int"
  , show (var "`f`" (undefined :: Int -> Int -> Int) :$ one) == "(1 `f`) :: Int -> Int"
  , show (var "`f`" (undefined :: Int -> Int -> Int) :$ one :$ two) == "1 `f` 2 :: Int"
  , show (one -?- two) == "1 ? 2 :: Int"
  , show (value "`compare`" (compare :: Int->Int->Ordering) :$ one) == "(1 `compare`) :: Int -> Ordering"
  , show (value "`compare`" (compare :: Int->Int->Ordering) :$ one :$ two) == "1 `compare` 2 :: Ordering"

  , holds n $ show . mapVars (\(Value ('_':s) d) -> Value (if null s then "_" else s) d) === show

  , show emptyString == "\"\" :: [Char]"
  , show (space -:- emptyString) == "\" \" :: [Char]"
  , show (space -:- ccs)         == "' ':cs :: [Char]"
  , show (ae -:- bee -:- emptyString) == "\"ab\" :: [Char]"
  , show (ae -:- bee -:- nilChar) == "['a','b'] :: [Char]"
  , show (ae -:- cc -:- nilChar) == "['a',c] :: [Char]"
  , show (ae -:- bee -:- ccs)         == "'a':'b':cs :: [Char]"
  , show (ae -:- space -:- bee -:- lineBreak -:- emptyString) == "\"a b\\n\" :: [Char]"
  , show (cc -:- space -:- dd -:- lineBreak -:- emptyString)  == "c:' ':d:\"\\n\" :: [Char]"
  , show (cc -:- space -:- dd -:- lineBreak -:- ccs)          == "c:' ':d:'\\n':cs :: [Char]"
  , show (cc -:- ae -:- bee -:- emptyString) == "c:\"ab\" :: [Char]"
  , show (cc -:- ae -:- bee -:- space -:- ae -:- bee -:- emptyString) == "c:\"ab ab\" :: [Char]"

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

  , show (one -:- nil)                    == "[1] :: [Int]"
  , show (zero -:- one -:- nil)           == "[0,1] :: [Int]"
  , show (minusOne -:- nil)               == "[-1] :: [Int]"
  , show (minusOne -:- minusTwo -:- nil)  == "[-1,-2] :: [Int]"
  , show (xx -:- minusTwo -:- yy -:- nil) == "[x,-2,y] :: [Int]"
  , show (xx -:- minusTwo -:- yy -:- xxs) == "x:(-2):y:xs :: [Int]"

  , show (ffE -$- zero)     == "f $ 0 :: Int"
  , show (ggE -$- xx)       == "g $ x :: Int"
  , show (ffE -$- minusOne) == "f $ (-1) :: Int"

  , holds n $ \e -> showExpr e `isPrefixOf` show e

  , show (if' pp xx yy)             == "(if p then x else y) :: Int"
  , show (if' false zero one)       == "(if False then 0 else 1) :: Int"
  , show (if' true two three)       == "(if True then 2 else 3) :: Int"
  , show (if' pp false true)        == "(if p then False else True) :: Bool"
  , show (not' (if' pp false true)) == "not (if p then False else True) :: Bool"
  , show (if' pp xx yy -*- zz)      == "(if p then x else y) * z :: Int"
  , show (zz -*- if' pp xx yy)      == "z * (if p then x else y) :: Int"
  , show (if' pp false true -||- if' qq true false)
    == "(if p then False else True) || (if q then True else False) :: Bool"
  , show (if' (null' xxs) zero (head' xxs -+- value "sum" (sum :: [Int] -> Int) :$ tail' xxs))
    == "(if null xs then 0 else head xs + sum (tail xs)) :: Int"
  , show (if' (xx -<- yy) (ff xx) (yy -*- zz)) == "(if x < y then f x else y * z) :: Int"

  , show (caseBool pp xx yy)             == "(case p of False -> x; True -> y) :: Int"
  , show (caseBool false zero one)       == "(case False of False -> 0; True -> 1) :: Int"
  , show (caseBool true two three)       == "(case True of False -> 2; True -> 3) :: Int"
  , show (caseBool pp false true)        == "(case p of False -> False; True -> True) :: Bool"
  , show (not' (caseBool pp false true)) == "not (case p of False -> False; True -> True) :: Bool"
  , show (caseBool pp xx yy -*- zz)      == "(case p of False -> x; True -> y) * z :: Int"
  , show (zz -*- caseBool pp xx yy)      == "z * (case p of False -> x; True -> y) :: Int"

  , showExpr (if' pp xx yy)             == "if p then x else y"
  , showExpr (if' false zero one)       == "if False then 0 else 1"
  , showExpr (if' true two three)       == "if True then 2 else 3"
  , showExpr (if' pp false true)        == "if p then False else True"
  , showExpr (if' (xx -<- yy) (ff xx) (yy -*- zz)) == "if x < y then f x else y * z"

  , showExpr (caseBool pp xx yy)             == "case p of False -> x; True -> y"
  , showExpr (caseBool false zero one)       == "case False of False -> 0; True -> 1"
  , showExpr (caseBool true two three)       == "case True of False -> 2; True -> 3"
  , showExpr (caseBool pp false true)        == "case p of False -> False; True -> True"
  , showExpr (caseBool pp true false)        == "case p of False -> True; True -> False"

  , show (caseOrdering (compare' xx yy) xx zz yy) == "(case compare x y of LT -> x; EQ -> z; GT -> y) :: Int"
  , show (caseOrdering (val LT) zero one two)     == "(case LT of LT -> 0; EQ -> 1; GT -> 2) :: Int"
  , show (caseOrdering (val GT) three four five)  == "(case GT of LT -> 3; EQ -> 4; GT -> 5) :: Int"
  , show (caseOrdering (compare' xx yy) (ff xx) zz (yy -+- zz)) == "(case compare x y of LT -> f x; EQ -> z; GT -> y + z) :: Int"

  , showExpr (caseOrdering (compare' xx yy) xx zz yy) == "case compare x y of LT -> x; EQ -> z; GT -> y"
  , showExpr (caseOrdering (val LT) zero one two)     == "case LT of LT -> 0; EQ -> 1; GT -> 2"
  , showExpr (caseOrdering (val GT) three four five)  == "case GT of LT -> 3; EQ -> 4; GT -> 5"
  , showExpr (caseOrdering (compare' xx yy) (ff xx) zz (yy -+- zz)) == "case compare x y of LT -> f x; EQ -> z; GT -> y + z"

  -- showing holes --
  , show (hole (undefined :: Int -> Int) :$ one)              == "_ 1 :: Int"
  , show (hole (undefined :: Int -> Int) :$ xx)               == "_ x :: Int"
  , show (hole (undefined :: Int -> Int -> Int) :$ one :$ xx) == "_ 1 x :: Int"
  , show (hole (undefined :: Int -> Int -> Int) :$ i_ :$ i_)  == "_ _ _ :: Int"

  -- A type --
  , show (hole (undefined :: A)) == "_ :: A"
  , show (val (0 :: A)) == "0 :: A"
  , show (val (1 :: A)) == "1 :: A"
  , show (val (2 :: A)) == "2 :: A"
  , show (var "x" (undefined :: A)) == "x :: A"
  , show (value "id" (id :: A -> A) :$ var "x" (undefined :: A)) == "id x :: A"

  -- B type --
  , show (hole (undefined :: B)) == "_ :: B"
  , show (val (0 :: B)) == "0 :: B"
  , show (val (1 :: B)) == "1 :: B"
  , show (val (2 :: B)) == "2 :: B"
  , show (var "x" (undefined :: B)) == "x :: B"
  , show (value "id" (id :: B -> B) :$ var "x" (undefined :: B)) == "id x :: B"

  -- [A] type --
  , show (hole (undefined :: [A])) == "_ :: [A]"
  , show (val ([0] :: [A])) == "[0] :: [A]"
  , show (val ([3,1] :: [A])) == "[3,1] :: [A]"
  , show (val ([0,1,2] :: [A])) == "[0,1,2] :: [A]"
  , show (var "xs" (undefined :: [A])) == "xs :: [A]"
  , show (value "id" (id :: [A] -> [A]) :$ var "xs" (undefined :: [A])) == "id xs :: [A]"

  , show (xx -:- nil -++- is_) == "x:([] ++ _) :: [Int]"
  , show (xx -:- yy -:- nil -++- is_) == "x:y:([] ++ _) :: [Int]"

  , show (cc -:- emptyString -++- cs_) == "c:(\"\" ++ _) :: [Char]"
  , show (cc -:- dd -:- emptyString -++- cs_) == "c:d:(\"\" ++ _) :: [Char]"
  , show (ae -:- bee -:- emptyString -++- cs_) == "'a':'b':(\"\" ++ _) :: [Char]"

  -- list pretty-printing only works with explicit val [] terminator
  -- other terminating consts are not unpacked
  , show (zero -:- one -:- two -:- nil) == "[0,1,2] :: [Int]"
  , show (zero -:- val [1,2::Int]) == "0:[1,2] :: [Int]"
  , show (xx -:- yy -:- three -:- four -:- nil) == "[x,y,3,4] :: [Int]"
  , show (xx -:- yy -:- val [3,4::Int]) == "x:y:[3,4] :: [Int]"
  ]
