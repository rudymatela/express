-- |
-- Module      : Data.Haexpress.Instances
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines utilities do deal with instances of typeclasses
module Data.Haexpress.Instances
  ( reifyEq
  , reifyOrd
  , reifyName

  , mkEq
  , mkOrd
  , mkOrdLessEqual
  , mkName
  , mkNameWith

-- old stuff that may go away: --
  , eqFor
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

reifyEq :: (Typeable a, Eq a) => a -> [Expr]
reifyEq a  =  mkEq  ((==) -:> a)

reifyOrd :: (Typeable a, Ord a) => a -> [Expr]
reifyOrd a  =  mkOrd (compare -:> a)

reifyName :: (Typeable a, Name a) => a -> [Expr]
reifyName a  =  mkName (name -:> a)

mkEq :: Typeable a => (a -> a -> Bool) -> [Expr]
mkEq (==)  =
  [ value "==" (==)
  , value "/=" (/=)
  ]
  where
  x /= y = not (x == y)

mkOrd :: Typeable a => (a -> a -> Ordering) -> [Expr]
mkOrd compare  =
  [ value "compare" compare
  , value "<=" (<=)
  , value "<" (<)
-- we don't include other Ord functions, at least for now
  ]
  where
  x <  y  =  x `compare` y == LT
  x <= y  =  x `compare` y /= GT

mkOrdLessEqual :: Typeable a => (a -> a -> Bool) -> [Expr]
mkOrdLessEqual (<=)  =
  [ value "<=" (<=)
  , value "<" (<)
-- TODO: include compare here for consistency with mkOrd
  ]
  where
  x < y  =  not (y <= x)

mkName :: Typeable a => (a -> String) -> [Expr]
mkName name  =  [value "name" name]

mkNameWith :: Typeable a => String -> a -> [Expr]
mkNameWith n a  =  [value "name" (const n -:> a)]


-- old stuff that may go away follows --

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

