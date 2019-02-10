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
  , evalInt
  , evalChar
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

  -- ** Integers
  , i_, xx, yy
  , zero, one, two, three, minusOne, minusTwo
  , idE, negateE, absE
  , id', negate', abs'
  , plusE, timesE
  , (-+-), (-*-)

  -- ** Chars
  , a, bee, cee, dee

  -- ** Lists
  , xxss
  )
where

import Data.Haexpress

evalError :: String -> a
evalError tn = error $ "evalInt: cannot evaluate Expr to " ++ tn ++ " type"

evalInt :: Expr -> Int
evalInt = eval $ evalError "Int"

evaluateInt :: Expr -> Maybe Int
evaluateInt = evaluate

evalChar :: Expr -> Char
evalChar = eval $ evalError "Char"

evaluateChar :: Expr -> Maybe Char
evaluateChar = evaluate

i_ :: Expr
i_  =  hole (undefined :: Int)

xx :: Expr
xx  =  var "x" (undefined :: Int)

yy :: Expr
yy  =  var "y" (undefined :: Int)

zero :: Expr
zero  =  val (0 :: Int)

one :: Expr
one  =  val (1 :: Int)

two :: Expr
two  =  val (2 :: Int)

three :: Expr
three  =  val (3 :: Int)

minusOne :: Expr
minusOne  =  val (-1 :: Int)

minusTwo :: Expr
minusTwo  =  val (-2 :: Int)

(-+-) :: Expr -> Expr -> Expr
e1 -+- e2 = plusE :$ e1 :$ e2
infixl 6 -+-

plusE :: Expr
plusE = value "+" ((+) :: Int -> Int -> Int)

(-*-) :: Expr -> Expr -> Expr
e1 -*- e2 = timesE :$ e1 :$ e2

timesE :: Expr
timesE  =  value "*" ((*) :: Int -> Int -> Int)

id' :: Expr -> Expr
id' e  =  idE :$ e

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

a :: Expr
a  =  val 'a'

bee :: Expr
bee  =  val 'b'

cee :: Expr
cee  =  val 'c'

dee :: Expr
dee  =  val 'd'

xxss :: Expr
xxss  =  var "xs" (undefined :: [Int])
