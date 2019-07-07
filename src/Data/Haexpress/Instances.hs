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
  , lookupNames

  , validApps
  , findValidApp

  , preludeNameInstances
  )
where

-- TODO: document and test functions of the Instances module

import Data.Haexpress.Basic
import Data.Haexpress.Name
import Data.Haexpress.Express
import Data.Haexpress.Utils.Typeable
import Data.Haexpress.Utils.List
import Data.Maybe


-- reifying instances --

-- | /O(1)./
-- Reifies an 'Eq' instance into a list of 'Expr's.
-- The list will contain '==' and '/=' for the given type.
-- (cf. 'mkEquation')
--
-- > > reifyEq (undefined :: Int)
-- > [ (==) :: Int -> Int -> Bool
-- > , (/=) :: Int -> Int -> Bool ]
--
-- > > reifyEq (undefined :: Bool)
-- > [ (==) :: Bool -> Bool -> Bool
-- > , (/=) :: Bool -> Bool -> Bool ]
--
-- > > reifyEq (undefined :: String)
-- > [ (==) :: [Char] -> [Char] -> Bool
-- > , (/=) :: [Char] -> [Char] -> Bool ]
reifyEq :: (Typeable a, Eq a) => a -> [Expr]
reifyEq a  =  mkEq  ((==) -:> a)

-- | /O(1)./
-- Reifies an 'Ord' instance into a list of 'Expr's.
-- The list will contain 'compare', '<=' and '<' for the given type.
-- (cf. 'mkComparisonLE', 'mkComparisonLT')
--
-- > > reifyOrd (undefined :: Int)
-- > [ compare :: Int -> Int -> Ordering
-- > , (<=) :: Int -> Int -> Bool
-- > , (<) :: Int -> Int -> Bool ]
--
-- > > reifyOrd (undefined :: Bool)
-- > [ compare :: Bool -> Bool -> Ordering
-- > , (<=) :: Bool -> Bool -> Bool
-- > , (<) :: Bool -> Bool -> Bool ]
--
-- > > reifyOrd (undefined :: [Bool])
-- > [ compare :: [Bool] -> [Bool] -> Ordering
-- > , (<=) :: [Bool] -> [Bool] -> Bool
-- > , (<) :: [Bool] -> [Bool] -> Bool ]
reifyOrd :: (Typeable a, Ord a) => a -> [Expr]
reifyOrd a  =  mkOrd (compare -:> a)

-- | /O(1)./
-- Reifies 'Eq' and 'Ord' instances into a list of 'Expr'.
reifyEqOrd :: (Typeable a, Ord a) => a -> [Expr]
reifyEqOrd a  =  reifyEq a ++ reifyOrd a

-- | /O(1)./
-- Reifies a 'Name' instance into a list of 'Expr's.
-- The list will contain 'name' for the given type.
-- (cf. 'lookupName', 'lookupNames')
--
-- > > reifyName (undefined :: Int)
-- > [name :: Int -> [Char]]
--
-- > > reifyName (undefined :: Bool)
-- > [name :: Bool -> [Char]]
reifyName :: (Typeable a, Name a) => a -> [Expr]
reifyName a  =  mkName (name -:> a)

-- todo: reifyExpr and related functions

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

lookupNames :: [Expr] -> Expr -> [String]
lookupNames is  =  variableNamesFromTemplate . lookupName is

listVarsWith :: [Expr] -> Expr -> [Expr]
listVarsWith is e  =  lookupName is e `listVarsAsTypeOf` e


-- helpers --

validApps :: [Expr] -> Expr -> [Expr]
validApps es e  =  mapMaybe ($$ e) es

findValidApp :: [Expr] -> Expr -> Maybe Expr
findValidApp es  =  listToMaybe . validApps es

(-:>) :: (a -> b) -> a -> (a -> b)
(-:>)  =  const
infixl 1 -:>


-- reified instances --

preludeNameInstances :: [Expr]
preludeNameInstances = concat
  [ reifyName (u :: ())
  , reifyName (u :: Bool)
  , reifyName (u :: Int)
  , reifyName (u :: Integer)
  , reifyName (u :: Char)
  , reifyName (u :: Ordering)
  , reifyName (u :: Rational)
  , reifyName (u :: Float)
  , reifyName (u :: Double)

  , reifyName (u :: [()])
  , reifyName (u :: [Bool])
  , reifyName (u :: [Int])
  , reifyName (u :: [Integer])
  , reifyName (u :: [Char])
  , reifyName (u :: [Ordering])
  , reifyName (u :: [Rational])
  , reifyName (u :: [Float])
  , reifyName (u :: [Double])

  , reifyName (u :: Maybe ())
  , reifyName (u :: Maybe Bool)
  , reifyName (u :: Maybe Int)
  , reifyName (u :: Maybe Integer)
  , reifyName (u :: Maybe Char)
  , reifyName (u :: Maybe Ordering)
  , reifyName (u :: Maybe Rational)
  , reifyName (u :: Maybe Float)
  , reifyName (u :: Maybe Double)

  , reifyName (u :: ((),()))
  , reifyName (u :: (Bool,Bool))
  , reifyName (u :: (Int,Int))
  , reifyName (u :: (Integer,Integer))
  , reifyName (u :: (Char,Char))
  , reifyName (u :: (Ordering,Ordering))
  , reifyName (u :: (Rational,Rational))
  , reifyName (u :: (Float,Float))
  , reifyName (u :: (Double,Double))
  ]
  where
  u :: a
  u  =  undefined

