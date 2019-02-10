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
  , zero, one, two, three, minusOne, minusTwo
  , idE, negateE, absE
  , plusE, timesE
  , (-+-), (-*-)

  -- ** Chars
--, a, bee, cee, dee
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

idE :: Expr
idE  =  value "id" (id :: Int -> Int)

negateE :: Expr
negateE  =  value "negate" (negate :: Int -> Int)

absE :: Expr
absE  =  value "abs" (abs :: Int -> Int)
