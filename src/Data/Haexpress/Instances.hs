-- |
-- Module      : Data.Haexpress.Instances
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines utilities do deal with instances of typeclasses
module Data.Haexpress.Instances
  ( eqFor
  , eqWith
  , compareFor
  , compareWith
  , nameFor
  , nameWith
  , exprFor
  , exprWith
  , validApps
  , preludeNameInstances
  )
where

import Data.Haexpress.Core
import Data.Haexpress.Name
import Data.Haexpress.Express
import Data.Typeable
import Data.Maybe

eqFor :: (Typeable a, Eq a) => a -> Expr
eqFor a  =  eqWith ((==) -:> a)

eqWith :: Typeable a => (a -> a -> Bool) -> Expr
eqWith (==)  =  value "==" (==)

compareFor :: (Typeable a, Ord a) => a -> Expr
compareFor a  =  compareWith (compare -:> a)

compareWith :: Typeable a => (a -> a -> Ordering) -> Expr
compareWith compare  =  value "compare" compare

nameFor :: (Typeable a, Name a) => a -> Expr
nameFor a  =  nameWith (name -:> a)

nameWith :: Typeable a => (a -> String) -> Expr
nameWith name  =  value "name" name

exprFor :: (Typeable a, Express a) => a -> Expr
exprFor a  =  exprWith (expr -:> a)

exprWith :: Typeable a => (a -> Expr) -> Expr
exprWith expr  =  value "expr" expr

validApps :: [Expr] -> Expr -> [Expr]
validApps es e = mapMaybe ($$ e) es

(-:>) :: (a -> b) -> a -> (a -> b)
(-:>)  =  const
infixl 1 -:>

preludeNameInstances :: [Expr]
preludeNameInstances =
  [ nameFor (u :: ())
  , nameFor (u :: Bool)
  , nameFor (u :: Int)
  , nameFor (u :: Integer)
  , nameFor (u :: Char)
  , nameFor (u :: Ordering)
  , nameFor (u :: Rational)
  , nameFor (u :: Float)
  , nameFor (u :: Double)

  , nameFor (u :: [()])
  , nameFor (u :: [Bool])
  , nameFor (u :: [Int])
  , nameFor (u :: [Integer])
  , nameFor (u :: [Char])
  , nameFor (u :: [Ordering])
  , nameFor (u :: [Rational])
  , nameFor (u :: [Float])
  , nameFor (u :: [Double])

  , nameFor (u :: Maybe ())
  , nameFor (u :: Maybe Bool)
  , nameFor (u :: Maybe Int)
  , nameFor (u :: Maybe Integer)
  , nameFor (u :: Maybe Char)
  , nameFor (u :: Maybe Ordering)
  , nameFor (u :: Maybe Rational)
  , nameFor (u :: Maybe Float)
  , nameFor (u :: Maybe Double)

  , nameFor (u :: ((),()))
  , nameFor (u :: (Bool,Bool))
  , nameFor (u :: (Int,Int))
  , nameFor (u :: (Integer,Integer))
  , nameFor (u :: (Char,Char))
  , nameFor (u :: (Ordering,Ordering))
  , nameFor (u :: (Rational,Rational))
  , nameFor (u :: (Float,Float))
  , nameFor (u :: (Double,Double))
  ]

u :: a
u  =  undefined

