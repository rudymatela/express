-- Copyright (c) 2019-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE TemplateHaskell #-}

import Test

import Data.Express.Utils.List

data Peano  =  Zero | Succ Peano deriving Show
data List a  =  a :- List a | Nil deriving Show
data Bush a  =  Bush a :-: Bush a | Leaf a deriving (Show, Eq)
data Tree a  =  Node (Tree a) a (Tree a) | Null deriving (Show, Eq)

instance Num Peano where
  Zero + n  =  n
  (Succ n) + m  =  Succ (n + m)
  Zero * n  =  Zero
  (Succ n) * m  =  m + n * m
  abs  =  id
  signum Zero  =  0
  signum (Succ n)  =  1
  fromInteger n  =  iterate Succ Zero !! fromInteger n
  Zero - m  =  Zero
  n - Zero  =  Zero
  Succ n - Succ m  =  n - m

deriveName ''Peano
deriveName ''List
deriveName ''Bush
deriveName ''Tree

-- Recursive nested datatype cascade
data RN      = RN RN0 (RN1 Int) (RN2 Int RN)
data RN0     = Nest0 Int | Recurse0 RN
data RN1 a   = Nest1 a   | Recurse1 RN
data RN2 a b = Nest2 a b | Recurse2 RN
deriveNameCascading ''RN

-- Those should have no effect (instance already exists):
{- uncommenting those should generate warnings
deriveName ''Bool
deriveName ''Maybe
deriveName ''Either
-}

-- Those should not generate warnings
deriveNameIfNeeded ''Bool
deriveNameIfNeeded ''Maybe
deriveNameIfNeeded ''Either

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , name (undefined :: Peano) == "x"
  , name (undefined :: List Int) == "l"
  , name (undefined :: Bush Int) == "b"
  , name (undefined :: Tree Int) == "t"
  , name (undefined :: List Bool) == "l"
  , name (undefined :: Bush Bool) == "b"
  , name (undefined :: Tree Bool) == "t"

  , name (undefined :: RN) == "r"
  , name (undefined :: RN0) == "r"
  , name (undefined :: RN1 Int) == "r"
  , name (undefined :: RN2 Bool Int) == "r"
  ]
