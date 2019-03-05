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

  , show plusE == "(+) :: Int -> Int -> Int"
  , show timesE == "(*) :: Int -> Int -> Int"
  , show (plusE :$ one) == "(1 +) :: Int -> Int"
  , show (timesE :$ two) == "(2 *) :: Int -> Int"

  , show (one -+- one)             == "1 + 1 :: Int"
  , show (minusOne -+- minusOne)   == "(-1) + (-1) :: Int"
  , show (minusTwo -*- two)        == "(-2) * 2 :: Int"
  , show (two -*- minusTwo)        == "2 * (-2) :: Int"
  , show (xx -+- (yy -+- zz))      == "x + (y + z) :: Int"

  , evl zero == (0 :: Int)
  , evl one  == (1 :: Int)
  , evl two   == (2 :: Int)
  , evl three == (3 :: Int)
  , evl minusOne == (-1 :: Int)
  , evl minusTwo == (-2 :: Int)

  , evl (one -+- one) == (2 :: Int)
  , evl (two -*- three) == (6 :: Int)
  , holds n $ \x y -> evl (val x -+- val y) == (x + y :: Int)
  , holds n $ \x y -> evl (val x -*- val y) == (x * y :: Int)
  , holds n $ \(IntE ex) (IntE ey) -> isGround ex && isGround ey ==> evl (ex -+- ey) == evl ex + (evl ey :: Int)
  , holds n $ \(IntE ex) (IntE ey) -> isGround ex && isGround ey ==> evl (ex -*- ey) == evl ex * (evl ey :: Int)

  , show xxs  == "xs :: [Int]"
  , show yys  == "ys :: [Int]"
  , show nilE == "[] :: [Int]"
  , show (unit one) == "[1] :: [Int]"

  -- Int -> Int --

  , show ffE == "f :: Int -> Int"
  , show ggE == "g :: Int -> Int"
  , show (ff xx) == "f x :: Int"
  , show (gg yy) == "g y :: Int"
  , show (ff one) == "f 1 :: Int"
  , show (gg minusTwo) == "g (-2) :: Int"

  , show idE == "id :: Int -> Int"
  , show negateE == "negate :: Int -> Int"
  , show absE == "abs :: Int -> Int"
  , show (id' yy) == "id y :: Int"
  , show (id' one) == "id 1 :: Int"
  , show (id' true) == "id True :: Bool"
  , show (negate' yy) == "negate y :: Int"
  , show (abs' xx') == "abs x' :: Int"

  , evl (id' one) == (1 :: Int)
  , evl (id' true) == (True :: Bool)
  , evl (negate' one) == (-1 :: Int)
  , evl (abs' minusTwo) == (2 :: Int)

  , evalInts nilE == []
  , evalInts (unit one) == [1]


  -- Char --

  , show c_ == "_ :: Char"
  , show bee == "'b' :: Char"
  , show cee == "'c' :: Char"
  , show dee == "'d' :: Char"

  , evl bee == 'b'
  , evl cee == 'c'
  , evl dee == 'd'


  -- [a] & [Int] --

  , show xxs == "xs :: [Int]"
  , show yys == "ys :: [Int]"
  , show nilE == "[] :: [Int]"
  , show consE == "(:) :: Int -> [Int] -> [Int]"
  , show (unit zero) == "[0] :: [Int]"
  , show (unit false) == "[False] :: [Bool]"

  , evl nilE == ([] :: [Int])
  , holds n $ \x -> evl (unit (val x)) == [x :: Int]
  , holds n $ \c -> evl (unit (val c)) == [c :: Char]
  , holds n $ \p -> evl (unit (val p)) == [p :: Bool]


  -- TODO: show "[Char]" as "String"?
  , show emptyStringE == "\"\" :: [Char]"
  , show (unit bee) == "\"b\" :: [Char]"

  , evl emptyStringE == ""
  , evl (unit bee) == "b"


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
