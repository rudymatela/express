-- |
-- Module      : Data.Haexpress.Fixtures
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines some 'Expr' fixtures to facilitate testing and playing around on
-- the REPL (GHCI).
module Data.Haexpress.Fixtures
  (
  -- * Convenience re-export
    module Data.Haexpress

  -- * Functions and values encoded as Expr or functions of Exprs
  -- | The naming rules are:
  --
  -- * Terminal values are named in words (e.g.: 'zero', 'bee', 'cee', 'dee', 'false').
  -- * Variables have the first character duplicated (e.g.: 'xx', 'yy', 'xxs');
  -- * Encoded functions are followed by @E@ (e.g.: 'idE', 'plusE');
  -- * Lifted functions are primed (e.g.: 'id'', 'negate'');
  -- * Lifted operators are surrounded by dashes (e.g.: '-+-', '-*-').

  -- ** Booleans
  , b_, pp, qq
  , false
  , true
  , notE
  , orE
  , andE
  , not'
  , (-||-)
  , (-&&-)

  -- ** Integers
  , i_, xx, yy, zz, xx'
  , zero, one, two, three, minusOne, minusTwo
  , idE, negateE, absE
  , id', negate', abs'
  , plusE, timesE
  , (-+-), (-*-)
  , ff, ffE
  , gg, ggE

  -- ** Chars
  , c_
  , bee, cee, dee

  -- ** Lists
  , (-:-)
  , xxs
  , yys
  , nilE
  , emptyStringE
  , unit
  , consE

  -- * Convenience monomorphically typed evaluation function aliases
  , evalBool
  , evalInt
  , evalInts
  , evalChar
  , evalString
  , evaluateBool
  , evaluateInt
  , evaluateInts
  , evaluateChar
  , evaluateString
  )
where

import Data.Haexpress
import Data.Maybe
import Data.Typeable (Typeable, typeOf)

-- | 'eval' bound to a 'Bool' result type
--
-- > > evalBool false
-- > False
--
-- > > evalBool zero
-- > *** Exception: evl: cannot evaluate Expr `0 :: Int' at the Bool type
evalBool :: Expr -> Bool
evalBool = evl

-- | 'evaluate' bound to a 'Bool' result type
--
-- > > evaluateBool false
-- > Just False
--
-- > > evaluateBool zero
-- > Nothing
evaluateBool :: Expr -> Maybe Bool
evaluateBool = evaluate

-- | 'eval' bound to a 'Int' result type
--
-- > > evalInt zero
-- > 0
--
-- > > evalInt false
-- > *** Exception: evl: cannot evaluate Expr `False :: Bool' at the Int type
evalInt :: Expr -> Int
evalInt = evl

-- | 'evaluate' bound to a 'Int' result type
--
-- > > evaluateInt zero
-- > Just 0
--
-- > > evaluateInt false
-- > Nothing
evaluateInt :: Expr -> Maybe Int
evaluateInt = evaluate

-- | 'eval' bound to a 'Char' result type
--
-- > > evalChar bee
-- > 'b'
--
-- > > evalChar zero
-- > *** Exception: evl: cannot evaluate Expr `0 :: Int' at the Char type
evalChar :: Expr -> Char
evalChar = evl

-- | 'evaluate' bound to a 'Char' result type
--
-- > > evaluateChar bee
-- > Just 'b'
-- > > evaluateChar zero
-- > Nothing
evaluateChar :: Expr -> Maybe Char
evaluateChar = evaluate

-- | 'eval' bound to a '[Int]' result type
--
-- > > evalInts (unit one)
-- > [1]
--
-- > > evalInts zero
-- > *** Exception: evl: cannot evaluate Expr `0 :: Int` at the [Int] type
evalInts :: Expr -> [Int]
evalInts = evl

-- | 'evaluate' bound to a '[Int]' result type
--
-- > > evaluateInts (zero -:- unit one)
-- > Just [0,1]
--
-- > > evaluateInts (bee -:- cee -:- unit dee)
-- > Nothing
evaluateInts :: Expr -> Maybe [Int]
evaluateInts = evaluate

-- | 'eval' bound to a 'String' result type
--
-- > > evalString (bee -:- cee -:- unit dee)
-- > "bcd"
--
-- > > evalString bee
-- > "*** Exception: evl: cannot evaluate Expr `'b' :: Char' at the [Char] type
evalString :: Expr -> String
evalString = evl

-- | 'evaluate' bound to a 'String' result type
--
-- > > evaluateString (zero -:- unit one)
-- > Nothing
--
-- > > evaluateString (bee -:- cee -:- unit dee)
-- > Just "bcd"
evaluateString :: Expr -> Maybe String
evaluateString = evaluate

-- | 'Expr' representing a hole of 'Bool' type.
--
-- > > b_
-- > _ :: Bool
b_ :: Expr
b_  =  hole (undefined :: Bool)

-- | 'Expr' representing a variable @p :: `Bool`@.
--
-- > > pp
-- > p :: Bool
pp :: Expr
pp  =  var "p" (undefined :: Bool)

-- | 'Expr' representing a variable @q :: `Bool`@.
--
-- > > qq
-- > q :: Bool
qq :: Expr
qq  =  var "q" (undefined :: Bool)

-- | 'False' encoded as an 'Expr'.
--
-- > > false
-- > False :: Bool
false :: Expr
false  =  val False

-- | 'True' encoded as an 'Expr'.
--
-- > > true
-- > True :: Bool
true :: Expr
true  =  val True

-- | The function 'not' encoded as an 'Expr'.
--
-- > > notE
-- > not :: Bool -> Bool
notE :: Expr
notE  =  value "not" not

-- | The function 'and' encoded as an 'Expr'.
--
-- > > andE
-- > (&&) :: Bool -> Bool -> Bool
andE :: Expr
andE  =  value "&&" (&&)

-- | The function 'or' encoded as an 'Expr'.
--
-- > > orE
-- > (||) :: Bool -> Bool -> Bool
orE :: Expr
orE  =  value "||" (||)

-- | The function 'not' lifted over the 'Expr' type.
--
-- > > not' false
-- > not False :: Bool
--
-- > > evalBool $ not' false
-- > True
--
-- > > not' pp
-- > not p :: Bool
not' :: Expr -> Expr
not' pp  =  notE :$ pp

-- | The function '&&' lifted over the 'Expr' type.
--
-- > > pp -&&- qq
-- > p && q :: Bool
--
-- > > false -&&- true
-- > False && True :: Bool
--
-- > > evalBool $ false -&&- true
-- > False
(-&&-) :: Expr -> Expr -> Expr
pp -&&- qq  =  andE :$ pp :$ qq

-- | The function '||' lifted over the 'Expr' type.
--
-- > > pp -||- qq
-- > p || q :: Bool
--
-- > > false -||- true
-- > False || True :: Bool
--
-- > > evalBool $ false -||- true
-- > True
(-||-) :: Expr -> Expr -> Expr
pp -||- qq  =  orE :$ pp :$ qq

-- | A typed hole of 'Int' type.
--
-- > > i_
-- > _ :: Int
i_ :: Expr
i_  =  hole (undefined :: Int)

-- | A variable @x@ of 'Int' type.
--
-- > > xx
-- > x :: Int
xx :: Expr
xx  =  var "x" (undefined :: Int)

-- | A variable @y@ of 'Int' type.
--
-- > > yy
-- > y :: Int
yy :: Expr
yy  =  var "y" (undefined :: Int)

-- | A variable @z@ of 'Int' type.
--
-- > > zz
-- > z :: Int
zz :: Expr
zz  =  var "z" (undefined :: Int)

-- | A variable @x'@ of 'Int' type.
--
-- > > xx'
-- > x' :: Int
xx' :: Expr
xx'  =  var "x'" (undefined :: Int)

-- | The value @0@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > zero
-- > 0 :: Int
zero :: Expr
zero  =  val (0 :: Int)

-- | The value @1@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > one
-- > 1 :: Int
one :: Expr
one  =  val (1 :: Int)

-- | The value @2@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > two
-- > 2 :: Int
two :: Expr
two  =  val (2 :: Int)

-- | The value @3@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > three
-- > 3 :: Int
three :: Expr
three  =  val (3 :: Int)

-- | The value @-1@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > minusOne
-- > -1 :: Int
minusOne :: Expr
minusOne  =  val (-1 :: Int)

-- | The value @-2@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > minusOne
-- > -2 :: Int
minusTwo :: Expr
minusTwo  =  val (-2 :: Int)

ff :: Expr -> Expr
ff = (ffE :$)

ffE :: Expr
ffE = var "f" (undefined :: Int -> Int)

gg :: Expr -> Expr
gg = (ggE :$)

ggE :: Expr
ggE = var "g" (undefined :: Int -> Int)

-- | The operator '+' for the 'Int' type for use on 'Expr's.  (See also 'plusE'.)
--
-- > > two -+- three
-- > 2 + 3 :: Int
--
-- > > minusOne -+- minusTwo -+- zero
-- > ((-1) + (-2)) + 0 :: Int
--
-- > > xx -+- (yy -+- zz)
-- > x + (y + z) :: Int
(-+-) :: Expr -> Expr -> Expr
e1 -+- e2 = plusE :$ e1 :$ e2
infixl 6 -+-

-- | The operator '+' for the 'Int' type.  (See also '-+-'.)
--
-- > > plusE
-- > (+) :: Int -> Int -> Int
--
-- > > plusE :$ one
-- > (1 +) :: Int -> Int
--
-- > > plusE :$ xx :$ yy
-- > x + y :: Int
plusE :: Expr
plusE = value "+" ((+) :: Int -> Int -> Int)

(-*-) :: Expr -> Expr -> Expr
e1 -*- e2 = timesE :$ e1 :$ e2

timesE :: Expr
timesE  =  value "*" ((*) :: Int -> Int -> Int)

-- | Constructs an application of 'id' as an 'Expr'.
--   Only works for 'Int', 'Bool', 'Char', 'String', @[Int]@, @[Bool]@.
--
-- > > id' yy
-- > id yy :: Int
--
-- > > id' one
-- > id 1 :: Int
--
-- > > eval 0 (id' one) :: Int
-- > 1
--
-- > > id' pp
-- > id p :: Bool
--
-- > > id' false
-- > id' False :: Bool
--
-- > > eval False $ id' true
-- > True :: Bool
id' :: Expr -> Expr
id' e  =  headOr err $ mapMaybe ($$ e) [ idE -- :: Int -> Int
                                       , value "id" (id :: Bool -> Bool)
                                       , value "id" (id :: Char -> Char)
                                       , value "id" (id :: [Int] -> [Int])
                                       , value "id" (id :: [Bool] -> [Bool])
                                       , value "id" (id :: String -> String)
                                       ]
  where
  err = error $ "id': unhandled type " ++ show (typ e)

-- | The function 'id' for the 'Int' type encoded as an 'Expr'.  (See also 'id''.)
--
-- > > idE :$ xx
-- > id x :: Int
--
-- > > idE :$ zero
-- > id 0 :: Int
--
-- > > evaluate $ idE :$ zero :: Maybe Int
-- > Just 0
idE :: Expr
idE  =  value "id" (id :: Int -> Int)

negate' :: Expr -> Expr
negate' e  =  negateE :$ e

negateE :: Expr
negateE  =  value "negate" (negate :: Int -> Int)

abs' :: Expr -> Expr
abs' e  =  absE :$ e

absE :: Expr
absE  =  value "abs" (abs :: Int -> Int)

c_ :: Expr
c_  =  hole (undefined :: Char)

-- | The character @\'b\'@ encoded as an 'Expr'
--
-- > > bee
-- > 'b' :: Char
--
-- > > eval 'z' bee
-- > 'b'
bee :: Expr
bee  =  val 'b'

-- | The character @\'c\'@ encoded as an 'Expr'
--
-- > > cee
-- > 'c' :: Char
--
-- > > eval 'z' cee
-- > 'c'
cee :: Expr
cee  =  val 'c'

-- | The character @\'d\'@ encoded as an 'Expr'
--
-- > > dee
-- > 'd' :: Char
--
-- > > eval 'x' dee
-- > 'd'
dee :: Expr
dee  =  val 'd'

-- | A variable named @xs@ of type @[Int]@ encoded as an 'Expr'.
--
-- > > xxs
-- > xs :: [Int]
xxs :: Expr
xxs  =  var "xs" (undefined :: [Int])

-- | A variable named @ys@ of type @[Int]@ encoded as an 'Expr'.
--
-- > > yys
-- > ys :: [Int]
yys :: Expr
yys  =  var "ys" (undefined :: [Int])

-- | An empty list of type @[Int]@ encoded as an 'Expr'.
--
-- > > nilE
-- > [] :: [Int]
nilE :: Expr
nilE  =  val ([] :: [Int])

-- | An empty 'String' encoded as an 'Expr'.
--
-- > > emptyStringE
-- > "" :: String
emptyStringE :: Expr
emptyStringE  =  val ""

-- | The list constructor with 'Int' as element type encoded as an 'Expr'.
--
-- > > consE
-- > (:) :: Int -> [Int] -> [Int]
--
-- > > consE :$ one :$ nilE
-- > [1] :: [Int]
--
-- Please prefer '-:-' and 'unit' when building lists of 'Expr'.
consE :: Expr
consE = value ":" ((:) :: Int -> [Int] -> [Int])

-- | 'unit' constructs a list with a single element.
--   This works for elements of type 'Int', 'Char' and 'Bool'.
--
-- > > unit one
-- > [1]
--
-- > > unit false
-- > [False]
unit :: Expr -> Expr
unit e  =  e -:- nil
  where
  nil | typ e == typ i_  =  nilE
      | typ e == typ c_  =  emptyStringE
      | typ e == typ b_  =  val ([] :: [Bool])

-- | The list constructor lifted over the 'Expr' type.
--   Works for the element types 'Int', 'Char' and 'Bool'.
--
-- > > zero -:- one -:- unit two
-- > [0,1,2] :: [Int]
--
-- > > zero -:- one -:- two -:- nilE
-- > [0,1,2] :: [Int]
--
-- > > bee -:- unit cee
-- > "bc" :: [Char]
(-:-) :: Expr -> Expr -> Expr
e1 -:- e2  =  cons :$ e1 :$ e2
  where
  cons | typ e1 == typ i_ = consE
       | typ e1 == typ c_ = value ":" ((:) :: Char -> String -> String)
       | typ e1 == typ b_ = value ":" ((:) :: Bool -> [Bool] -> [Bool])
infixr 5 -:-

(-++-) :: Expr -> Expr -> Expr
e1 -++- e2 = append :$ e1 :$ e2
  where
  append | typ e1 == typ i_ = value "++" ((++) :: [Int] -> [Int] -> [Int])
         | typ e1 == typ c_ = value "++" ((++) :: String -> String -> String)
         | typ e1 == typ b_ = value "++" ((++) :: [Bool] -> [Bool] -> [Bool])
infixr 5 -++-
-- TODO: make the above work for different types

head' :: Expr -> Expr
head' exs = headE :$ exs where headE = value "head" (head :: [Int] -> Int)
-- TODO: make head and tail work for lists of chars and lists of bools

tail' :: Expr -> Expr
tail' exs = tailE :$ exs where tailE = value "tail" (tail :: [Int] -> [Int])

headOr :: a -> [a] -> a
headOr x []     =  x
headOr _ (x:_)  =  x
