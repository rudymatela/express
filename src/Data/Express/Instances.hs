-- |
-- Module      : Data.Express.Instances
-- Copyright   : (c) 2019-2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines utilities do deal with instances of typeclasses
--
-- Functions provided by this module store the set of instances as a simple
-- Haskell list.  When storing only a few instances this should be fine in
-- terms of performance.
--
-- If you plan to store hundreds or thousands of instances,
-- we recommend implementing different versions that use
-- a more efficient Set/Map storage.
module Data.Express.Instances
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

import Data.Express.Basic
import Data.Express.Name
import Data.Express.Express
import Data.Express.Utils.Typeable
import Data.Express.Utils.List
import Data.Maybe
import Control.Applicative ((<$>)) -- for GHC <= 7.8


-- reifying instances --

-- | /O(1)./
-- Reifies an 'Eq' instance into a list of 'Expr's.
-- The list will contain '==' and '/=' for the given type.
-- (cf. 'mkEq', 'mkEquation')
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
-- (cf. 'mkOrd', 'mkOrdLessEqual', 'mkComparisonLE', 'mkComparisonLT')
--
-- > > reifyOrd (undefined :: Int)
-- > [ (<=) :: Int -> Int -> Bool
-- > , (<) :: Int -> Int -> Bool ]
--
-- > > reifyOrd (undefined :: Bool)
-- > [ (<=) :: Bool -> Bool -> Bool
-- > , (<) :: Bool -> Bool -> Bool ]
--
-- > > reifyOrd (undefined :: [Bool])
-- > [ (<=) :: [Bool] -> [Bool] -> Bool
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
-- (cf. 'mkName', 'lookupName', 'lookupNames')
--
-- > > reifyName (undefined :: Int)
-- > [name :: Int -> [Char]]
--
-- > > reifyName (undefined :: Bool)
-- > [name :: Bool -> [Char]]
reifyName :: (Typeable a, Name a) => a -> [Expr]
reifyName a  =  mkName (name -:> a)

-- | /O(1)/.
-- Builds a reified 'Eq' instance from the given '==' function.
-- (cf. 'reifyEq')
--
-- > > mkEq ((==) :: Int -> Int -> Bool)
-- > [ (==) :: Int -> Int -> Bool
-- > , (/=) :: Int -> Int -> Bool ]
mkEq :: Typeable a => (a -> a -> Bool) -> [Expr]
mkEq (==)  =
  [ value "==" (==)
  , value "/=" (/=)
  ]
  where
  x /= y = not (x == y)

-- | /O(1)/.
-- Builds a reified 'Ord' instance from the given 'compare' function.
-- (cf. 'reifyOrd', 'mkOrdLessEqual')
mkOrd :: Typeable a => (a -> a -> Ordering) -> [Expr]
mkOrd compare  =
  [ value "<=" (<=)
  , value "<" (<)
-- we don't include other Ord functions, at least for now
--, value "compare" compare
  ]
  where
  x <  y  =  x `compare` y == LT
  x <= y  =  x `compare` y /= GT

-- | /O(1)/.
-- Builds a reified 'Ord' instance from the given '<=' function.
-- (cf. 'reifyOrd', 'mkOrd')
mkOrdLessEqual :: Typeable a => (a -> a -> Bool) -> [Expr]
mkOrdLessEqual (<=)  =
  [ value "<=" (<=)
  , value "<" (<)
  ]
  where
  x < y  =  not (y <= x)

-- | /O(1)/.
-- Builds a reified 'Name' instance from the given 'name' function.
-- (cf. 'reifyName', 'mkNameWith')
mkName :: Typeable a => (a -> String) -> [Expr]
mkName name  =  [value "name" name]

-- | /O(1)/.
-- Builds a reified 'Name' instance from the given 'String' and type.
-- (cf. 'reifyName', 'mkName')
mkNameWith :: Typeable a => String -> a -> [Expr]
mkNameWith n a  =  [value "name" (const n -:> a)]


-- searching for functions --

-- | /O(n)./
-- Lookups for a comparison function (@:: a -> a -> Bool@)
-- with the given name and argument type.
lookupComparison :: String -> TypeRep -> [Expr] -> Maybe Expr
lookupComparison n' t  =  find (\i@(Value n _) -> n == n' && typ i == mkComparisonTy t)

-- | /O(n)./
-- Returns whether an 'Eq' instance exists in the given instances list
-- for the given 'TypeRep'.
--
-- > > isEqT (reifyEqOrd (undefined :: Int)) (typeOf (undefined :: Int))
-- > True
--
-- > > isEqT (reifyEqOrd (undefined :: Int)) (typeOf (undefined :: [[[Int]]]))
-- > False
--
-- Given that the instances list has length /n/, this function is /O(n)/.
isEqT :: [Expr] -> TypeRep -> Bool
isEqT is t  =  isJust $ lookupComparison "==" t is

-- | /O(n)./
-- Returns whether an 'Ord' instance exists in the given instances list
-- for the given 'TypeRep'.
--
-- > > isOrdT (reifyEqOrd (undefined :: Int)) (typeOf (undefined :: Int))
-- > True
--
-- > > isOrdT (reifyEqOrd (undefined :: Int)) (typeOf (undefined :: [[[Int]]]))
-- > False
--
-- Given that the instances list has length /n/, this function is /O(n)/.
isOrdT :: [Expr] -> TypeRep -> Bool
isOrdT is t  =  isJust $ lookupComparison "<=" t is

-- | /O(n)./
-- Returns whether both 'Eq' and 'Ord' instance exist in the given list
-- for the given 'TypeRep'.
--
-- Given that the instances list has length /n/, this function is /O(n)/.
isEqOrdT :: [Expr] -> TypeRep -> Bool
isEqOrdT is t  =  isEqT is t && isOrdT is t

-- | /O(n+m)./
-- Returns whether an 'Eq' instance exists in the given instances list
-- for the given 'Expr'.
--
-- > > isEq (reifyEqOrd (undefined :: Int)) (val (0::Int))
-- > True
--
-- > > isEq (reifyEqOrd (undefined :: Int)) (val ([[[0::Int]]]))
-- > False
--
-- Given that the instances list has length /m/
-- and that the given 'Expr' has size /n/,
-- this function is /O(n+m)/.
isEq :: [Expr] -> Expr -> Bool
isEq is  =  isEqT is . typ

-- | /O(n+m)./
-- Returns whether an 'Ord' instance exists in the given instances list
-- for the given 'Expr'.
--
-- > > isOrd (reifyEqOrd (undefined :: Int)) (val (0::Int))
-- > True
--
-- > > isOrd (reifyEqOrd (undefined :: Int)) (val ([[[0::Int]]]))
-- > False
--
-- Given that the instances list has length /m/
-- and that the given 'Expr' has size /n/,
-- this function is /O(n+m)/.
isOrd :: [Expr] -> Expr -> Bool
isOrd is  =  isOrdT is . typ

-- | /O(n+m)./
-- Returns whether both 'Eq' and 'Ord' instance exist in the given list
-- for the given 'Expr'.
--
-- Given that the instances list has length /m/
-- and that the given 'Expr' has size /n/,
-- this function is /O(n+m)/.
isEqOrd :: [Expr] -> Expr -> Bool
isEqOrd is e  =  isEq is e && isOrd is e

-- | /O(n+m)./
-- Like 'mkEquation', 'mkComparisonLE' and 'mkComparisonLT'
-- but allows providing the binary operator name.
--
-- When not possible, this function returns 'False' encoded as an 'Expr'.
mkComparison :: String -> [Expr] -> Expr -> Expr -> Expr
mkComparison n' is e1 e2  =  fromMaybe (val False) $ do
  e1e <- findValidApp os e1
  e1e $$ e2
  where
  os = [eq | eq@(Value n _) <- is, n == n']

-- | /O(n+m)./
-- Returns an equation between two expressions
-- given that it is possible to do so from '==' operators
-- given in the argument instances list.
--
-- When not possible, this function returns 'False' encoded as an 'Expr'.
mkEquation :: [Expr] -> Expr -> Expr -> Expr
mkEquation  =  mkComparison "=="

-- | /O(n+m)./
-- Returns a less-than inequation between two expressions
-- given that it is possible to do so from '<' operators
-- given in the argument instances list.
--
-- When not possible, this function returns 'False' encoded as an 'Expr'.
mkComparisonLT :: [Expr] -> Expr -> Expr -> Expr
mkComparisonLT  =  mkComparison "<"

-- | /O(n+m)./
-- Returns a less-than-or-equal-to inequation between two expressions
-- given that it is possible to do so from '<=' operators
-- given in the argument instances list.
--
-- When not possible, this function returns 'False' encoded as an 'Expr'.
mkComparisonLE :: [Expr] -> Expr -> Expr -> Expr
mkComparisonLE  =  mkComparison "<="

-- | /O(n+m)./
-- Like 'name' but lifted over an instance list and an 'Expr'.
--
-- > > lookupName preludeNameInstances (val False)
-- > "p"
--
-- > > lookupName preludeNameInstances (val (0::Int))
-- > "x"
--
-- This function defaults to @"x"@ when no appropriate 'name' is found.
--
-- > > lookupName [] (val False)
-- > "x"
lookupName :: [Expr] -> Expr -> String
lookupName is e  =  fromMaybe d $ eval "x" <$> findValidApp es e
  where
  t  =  typ e
  d | isFunTy t  =  "f"
    | otherwise  =  'x' : replicate (countListTy t) 's'
  es = [e | e@(Value "name" _) <- is]

-- | /O(n+m)./
-- A mix between 'lookupName' and 'names':
-- this returns an infinite list of names
-- based on an instances list and an 'Expr'.
lookupNames :: [Expr] -> Expr -> [String]
lookupNames is  =  variableNamesFromTemplate . lookupName is

-- | /O(n+m)./
-- Like 'lookupNames' but returns a list of variables encoded as 'Expr's.
listVarsWith :: [Expr] -> Expr -> [Expr]
listVarsWith is e  =  lookupName is e `listVarsAsTypeOf` e


-- helpers --

-- |
-- Given a list of functional expressions and another expression,
-- returns a list of valid applications.
validApps :: [Expr] -> Expr -> [Expr]
validApps es e  =  mapMaybe ($$ e) es

-- |
-- Like 'validApps' but returns a 'Maybe' value.
findValidApp :: [Expr] -> Expr -> Maybe Expr
findValidApp es  =  listToMaybe . validApps es

(-:>) :: (a -> b) -> a -> (a -> b)
(-:>)  =  const
infixl 1 -:>


-- reified instances --

-- |
-- A list of reified 'Name' instances
-- for an arbitrary selection of types from the Haskell "Prelude".
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

  , reifyName (u :: () -> ())
  , reifyName (u :: Bool -> Bool)
  , reifyName (u :: Int -> Int)
  , reifyName (u :: Integer -> Integer)
  , reifyName (u :: Char -> Char)
  , reifyName (u :: Ordering -> Ordering)
  , reifyName (u :: Rational -> Rational)
  , reifyName (u :: Float -> Float)
  , reifyName (u :: Double -> Double)

  , reifyName (u :: () -> () -> ())
  , reifyName (u :: Bool -> Bool -> Bool)
  , reifyName (u :: Int -> Int -> Int)
  , reifyName (u :: Integer -> Integer -> Integer)
  , reifyName (u :: Char -> Char -> Char)
  , reifyName (u :: Ordering -> Ordering -> Ordering)
  , reifyName (u :: Rational -> Rational -> Rational)
  , reifyName (u :: Float -> Float -> Float)
  , reifyName (u :: Double -> Double -> Double)
  ]
  where
  u :: a
  u  =  undefined

