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

  -- * Convenience monomorphically typed evaluation function aliases
  , evalBool
  , evalInt
  , evalChar
  , evaluateBool
  , evaluateInt
  , evaluateChar

  -- * Functions and values encoded as 'Expr' or functions of 'Expr's
  -- | The naming rules are:
  --
  -- * Terminal values are named in words (e.g.: 'zero', 'bee', 'cee', 'dee').
  -- * Variables have their characters duplicated (e.g.: 'xx', 'xxss');
  -- * Functions encoded as expressions are followed by "E" (e.g.: 'idE', 'plusE');
  -- * Functions over expressions are primed (e.g.: 'id'', 'negate'');
  -- * Operators are surrounded by dashes (e.g.: '-+-', '-*-').

  -- ** Booleans
  , b_, pp, qq
  , falseE
  , trueE
  , orE
  , andE
  , (-||-)
  , (-&&-)

  -- ** Integers
  , i_, xx, yy, zz, xx''
  , zero, one, two, three, minusOne, minusTwo
  , idE, negateE, absE
  , id', negate', abs'
  , plusE, timesE
  , (-+-), (-*-)
  , ff, ffE
  , gg, ggE

  -- ** Chars
  , bee, cee, dee

  -- ** Lists
  , xxss
  )
where

import Data.Haexpress
import Data.Maybe

evalError :: String -> a
evalError tn = error $ "evalInt: cannot evaluate Expr to " ++ tn ++ " type"

evalBool :: Expr -> Bool
evalBool = eval $ evalError "Bool"

evaluateBool :: Expr -> Maybe Bool
evaluateBool = evaluate

evalInt :: Expr -> Int
evalInt = eval $ evalError "Int"

evaluateInt :: Expr -> Maybe Int
evaluateInt = evaluate

evalChar :: Expr -> Char
evalChar = eval $ evalError "Char"

evaluateChar :: Expr -> Maybe Char
evaluateChar = evaluate

-- | 'Expr' representing a hole of 'Bool' type.
b_ :: Expr
b_  =  hole (undefined :: Bool)

-- | 'Expr' representing a variable @p :: `Bool`@.
pp :: Expr
pp  =  var "p" (undefined :: Bool)

-- | 'Expr' representing a variable @q :: `Bool`@.
qq :: Expr
qq  =  var "q" (undefined :: Bool)

falseE :: Expr
falseE  =  val False

trueE :: Expr
trueE  =  val True

notE :: Expr
notE  =  value "not" not

andE :: Expr
andE  =  value "&&" (&&)

orE :: Expr
orE  =  value "||" (||)

(-&&-) :: Expr -> Expr -> Expr
pp -&&- qq  =  andE :$ pp :$ qq

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
-- > > xx''
-- > x' :: Int
xx'' :: Expr
xx''  =  var "x'" (undefined :: Int)

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
-- > > id' falseE
-- > id' False :: Bool
--
-- > > eval False $ id' trueE
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
-- > > xxss
-- > xs :: [Int]
xxss :: Expr
xxss  =  var "xs" (undefined :: [Int])

headOr :: a -> [a] -> a
headOr x []     =  x
headOr _ (x:_)  =  x
