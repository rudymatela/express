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
  , reifyEqOrd
  , reifyName

  , mkEq
  , mkOrd
  , mkOrdLessEqual
  , mkName
  , mkNameWith

  , isEq
  , isOrd
  , isEqOrd
  , isEqT
  , isOrdT
  , isEqOrdT

  , mkEquation
  , mkComparisonLE
  , mkComparisonLT
  , mkComparison
  , lookupComparison

  , listVarsWith
  , lookupName

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
  , findValidApp
  , preludeNameInstances
  )
where

import Data.Haexpress.Basic
import Data.Haexpress.Name
import Data.Haexpress.Express
import Data.Haexpress.Utils.Typeable
import Data.Haexpress.Utils.List
import Data.Maybe


-- reifying instances --

reifyEq :: (Typeable a, Eq a) => a -> [Expr]
reifyEq a  =  mkEq  ((==) -:> a)

reifyOrd :: (Typeable a, Ord a) => a -> [Expr]
reifyOrd a  =  mkOrd (compare -:> a)

reifyEqOrd :: (Typeable a, Ord a) => a -> [Expr]
reifyEqOrd a  =  reifyEq a ++ reifyOrd a

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


-- searching for functions --

lookupComparison :: String -> TypeRep -> [Expr] -> Maybe Expr
lookupComparison n' t  =  find (\i@(Value n _) -> n == n' && typ i == mkComparisonTy t)

isEqT :: [Expr] -> TypeRep -> Bool
isEqT is t  =  isJust $ lookupComparison "==" t is

isOrdT :: [Expr] -> TypeRep -> Bool
isOrdT is t  =  isJust $ lookupComparison "<=" t is

isEqOrdT :: [Expr] -> TypeRep -> Bool
isEqOrdT is t  =  isEqT is t && isOrdT is t

isEq :: [Expr] -> Expr -> Bool
isEq is  =  isEqT is . typ

isOrd :: [Expr] -> Expr -> Bool
isOrd is  =  isOrdT is . typ

isEqOrd :: [Expr] -> Expr -> Bool
isEqOrd is e  =  isEq is e && isOrd is e

mkComparison :: String -> [Expr] -> Expr -> Expr -> Maybe Expr
mkComparison n' is e1 e2  =  do
  e1e <- findValidApp os e1
  e1e $$ e2
  where
  os = [eq | eq@(Value n _) <- is, n == n']

mkEquation :: [Expr] -> Expr -> Expr -> Maybe Expr
mkEquation  =  mkComparison "=="

mkComparisonLT :: [Expr] -> Expr -> Expr -> Maybe Expr
mkComparisonLT  =  mkComparison "<"

mkComparisonLE :: [Expr] -> Expr -> Expr -> Maybe Expr
mkComparisonLE  =  mkComparison "<="

lookupName :: [Expr] -> Expr -> String
lookupName is e  =  fromMaybe "x" $ eval "x" <$> findValidApp es e
  where
  es = [e | e@(Value "name" _) <- is]

listVarsWith :: [Expr] -> Expr -> [Expr]
listVarsWith is e  =  lookupName is e `listVarsAsTypeOf` e

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
validApps es e  =  mapMaybe ($$ e) es

findValidApp :: [Expr] -> Expr -> Maybe Expr
findValidApp es  =  listToMaybe . validApps es

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
  where
  u :: a
  u  =  undefined

