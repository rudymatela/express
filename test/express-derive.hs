-- Copyright (c) 2019-2021 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Test


data Choice  =  Ae | Bee | Cee deriving (Show, Eq, Typeable)
data Peano  =  Zero | Succ Peano deriving (Show, Eq, Typeable)
data List a  =  a :- List a | Nil deriving (Show, Eq, Typeable)
data Bush a  =  Bush a :-: Bush a | Leaf a deriving (Show, Eq, Typeable)
data Tree a  =  Node (Tree a) a (Tree a) | Null deriving (Show, Eq, Typeable)

deriveExpress ''Choice
deriveExpress ''Peano
deriveExpress ''List
deriveExpress ''Bush
deriveExpress ''Tree

deriveListable ''Choice
deriveListable ''Peano
deriveListable ''List
deriveListable ''Bush
deriveListable ''Tree

-- Nested datatype cascade
data Nested  =  Nested N0 (N1 Int) (N2 Int Int) deriving (Eq, Show, Typeable)
data N0      =  R0 Int deriving (Eq, Show, Typeable)
data N1 a    =  R1 a   deriving (Eq, Show, Typeable)
data N2 a b  =  R2 a b deriving (Eq, Show, Typeable)

deriveExpressCascading ''Nested
deriveListableCascading ''Nested

-- Recursive nested datatype cascade
data RN       =  RN RN0 (RN1 Int) (RN2 Int RN) deriving (Eq, Show, Typeable)
data RN0      =  Nest0 Int | Recurse0 RN deriving (Eq, Show, Typeable)
data RN1 a    =  Nest1 a   | Recurse1 RN deriving (Eq, Show, Typeable)
data RN2 a b  =  Nest2 a b | Recurse2 RN deriving (Eq, Show, Typeable)
-- beware: values of the above type are always infinite!
--         derivation works but full evaluation does not terminate

deriveExpressCascading ''RN
deriveListableCascading ''RN

-- Those should have no effect (instance already exists):
{- uncommenting those should generate warnings
deriveExpress ''Bool
deriveExpress ''Maybe
deriveExpress ''Either
-}

-- Those should not generate warnings
deriveExpressIfNeeded ''Bool
deriveExpressIfNeeded ''Maybe
deriveExpressIfNeeded ''Either

data Mutual    =  Mutual0   | Mutual CoMutual deriving (Eq, Show, Typeable)
data CoMutual  =  CoMutual0 | CoMutual Mutual deriving (Eq, Show, Typeable)

deriveListableCascading ''Mutual
deriveExpressCascading ''Mutual


main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , holds n (exprIsVal :: Choice -> Bool)
  , fails n (exprIsVal :: Peano -> Bool)

  , fails n (exprIsVal :: List Int -> Bool)
  , fails n (exprIsVal :: List Bool -> Bool)

  , fails n (exprIsVal :: Bush Int -> Bool)
  , fails n (exprIsVal :: Bush Bool -> Bool)

  , fails n (exprIsVal :: Tree Int -> Bool)
  , fails n (exprIsVal :: Tree Bool -> Bool)

  , holds n (exprIsValUnderEvaluate :: Choice -> Bool)
  , holds n (exprIsValUnderEvaluate :: Peano -> Bool)

  , holds n (exprIsValUnderEvaluate :: List Int -> Bool)
  , holds n (exprIsValUnderEvaluate :: List Bool -> Bool)

  , holds n (exprIsValUnderEvaluate :: Bush Int -> Bool)
  , holds n (exprIsValUnderEvaluate :: Bush Bool -> Bool)

  , holds n (exprIsValUnderEvaluate :: Tree Int -> Bool)
  , holds n (exprIsValUnderEvaluate :: Tree Bool -> Bool)

  , holds n (exprIsValUnderEvaluate :: Nested -> Bool)
  , holds n (exprIsValUnderEvaluate :: N0 -> Bool)
  , holds n (exprIsValUnderEvaluate :: N1 Int -> Bool)
  , holds n (exprIsValUnderEvaluate :: N2 Int Bool -> Bool)

  , holds n (exprIsValUnderEvaluate :: Mutual -> Bool)
  , holds n (exprIsValUnderEvaluate :: CoMutual -> Bool)
  ]

-- not true in all cases
exprIsVal :: (Listable a, Express a, Show a) => a -> Bool
exprIsVal x  =  expr x == val x

exprIsValUnderEvaluate :: (Listable a, Express a, Show a, Eq a) => a -> Bool
exprIsValUnderEvaluate x  =  evaluate (expr x) == (evaluate (val x) -: mayb x)
