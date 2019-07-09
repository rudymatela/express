-- |
-- Module      : Data.Haexpress.Name
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Name' type class.
module Data.Haexpress.Name
  ( Name (..)
  , names
  , variableNamesFromTemplate
  )
where

import Data.Haexpress.Utils.String

import Data.Char
import Data.List
import Data.Maybe (fromJust)
import Data.Either (fromLeft, fromRight)
import Data.Ratio (Ratio)

-- |
-- If we were to come up with a variable name for the given type
-- what 'name' would it be?
--
-- An instance for a given type @ Ty @ is simply given by:
--
-- > instance Name Ty where name _ = "x"
--
-- Examples:
--
-- > > name (undefined :: Int)
-- > "x"
--
-- > > name (undefined :: Bool)
-- > "p"
--
-- > > name (undefined :: [Int])
-- > "xs"
--
-- This is then used to generate an infinite list of variable 'names':
--
-- > > names (undefined :: Int)
-- > ["x", "y", "z", "x'", "y'", "z'", "x''", "y''", "z''", ...]
--
-- > > names (undefined :: Bool)
-- > ["p", "q", "r", "p'", "q'", "r'", "p''", "q''", "r''", ...]
--
-- > > names (undefined :: [Int])
-- > ["xs", "ys", "zs", "xs'", "ys'", "zs'", "xs''", "ys''", ...]
class Name a where
  -- | /O(1)./
  --
  -- Returns a name for a variable of the given argument's type.
  --
  -- > > name (undefined :: Int)
  -- > "x"
  --
  -- > > name (undefined :: [Bool])
  -- > "ps"
  --
  -- > > name (undefined :: [Maybe Integer])
  -- > "mxs"
  --
  -- The default definition is:
  --
  -- > name _ = "x"
  name :: a -> String
  name _ = "x"

-- |
-- > name (undefined :: ()) = "u"
-- > names (undefined :: ()) = ["u", "v", "w", "u'", "v'", ...]
instance Name ()        where  name _  =  "u"

-- |
-- > name (undefined :: Bool) = "p"
-- > names (undefined :: Bool) = ["p", "q", "r", "p'", "q'", ...]
instance Name Bool      where  name _  =  "p"

-- |
-- > name (undefined :: Int) = "x"
-- > names (undefined :: Int) = ["x", "y", "z", "x'", "y'", ...]
instance Name Int       where  name _  =  "x"

-- |
-- > name (undefined :: Integer) = "x"
-- > names (undefined :: Integer) = ["x", "y", "z", "x'", ...]
instance Name Integer   where  name _  =  "x"

-- |
-- > name (undefined :: Char) = "c"
-- > names (undefined :: Char) = ["c", "d", "e", "c'", "d'", ...]
instance Name Char      where  name _  =  "c"

-- |
-- > name (undefined :: Ordering) = "o"
-- > names (undefined :: Ordering) = ["o", "p", "q", "o'", ...]
instance Name Ordering  where  name _  =  "o"

-- |
-- > name (undefined :: Rational) = "q"
-- > names (undefined :: Rational) = ["q", "r", "s", "q'", ...]
instance Name (Ratio a) where  name _  =  "q"

-- |
-- > name (undefined :: Float) = "x"
-- > names (undefined :: Float) = ["x", "y", "z", "x'", ...]
instance Name Float     where  name _  =  "x"

-- |
-- > name (undefined :: Double) = "x"
-- > names (undefined :: Double) = ["x", "y", "z", "x'", ...]
instance Name Double    where  name _  =  "x"

-- |
-- > names (undefined :: ()->()) = ["f", "g", "h", "f'", ...]
-- > names (undefined :: Int->Int) = ["f", "g", "h", ...]
instance Name (a -> b)  where  name _  =  "f"

-- |
-- > names (undefined :: Maybe Int) = ["mx", "mx1", "mx2", ...]
-- > nemes (undefined :: Maybe Bool) = ["mp", "mp1", "mp2", ...]
instance Name a => Name (Maybe a) where
  name mx  =  "m" ++ name (fromJust mx)

-- |
-- > names (undefined :: Either Int Int) = ["exy", "exy1", ...]
-- > names (undefined :: Either Int Bool) = ["exp", "exp1", ...]
instance (Name a, Name b) => Name (Either a b) where
  name exy  =  "e" ++ n ++ m
    where
    x = fromLeft undefined exy
    y = fromRight undefined exy
    n = name x
    m = head $ names y \\ [n]

-- |
-- > names (undefined :: (Int,Int)) = ["xy", "zw", "xy'", ...]
-- > names (undefined :: (Bool,Bool)) = ["pq", "rs", "pq'", ...]
instance (Name a, Name b) => Name (a,b) where
  name xy  =  n ++ m
    where
    (x,y)  =  xy
    n  =  name x
    m  =  head $ names y \\ [n]

-- |
-- > names (undefined :: (Int,Int,Int)) = ["xyz","uvw", ...]
-- > names (undefined :: (Int,Bool,Char)) = ["xpc", "xpc1", ...]
instance (Name a, Name b, Name c) => Name (a,b,c) where
  name xyz  =  n ++ m ++ o
    where
    (x,y,z)  =  xyz
    n  =  name x
    m  =  head $ names y \\ [n]
    o  =  head $ names z \\ [n,m]

-- |
-- > names (undefined :: ((),(),(),())) = ["uuuu", "uuuu1", ...]
-- > names (undefined :: (Int,Int,Int,Int)) = ["xxxx", ...]
instance (Name a, Name b, Name c, Name d) => Name (a,b,c,d) where
  name xyzw  =  name x ++ name y ++ name z ++ name w  where  (x,y,z,w)  =  xyzw

-- |
-- > names (undefined :: [Int]) = ["xs", "ys", "zs", "xs'", ...]
-- > names (undefined :: [Bool]) = ["ps", "qs", "rs", "ps'", ...]
instance Name a => Name [a] where
  name xs  =  name (head xs) ++ "s"

-- |
-- Returns na infinite list of variable names from the given type:
-- the result of 'variableNamesFromTemplate' after 'name'.
--
-- > > names (undefined :: Int)
-- > ["x", "y", "z", "x'", "y'", "z'", "x''", "y''", "z''", ...]
--
-- > > names (undefined :: Bool)
-- > ["p", "q", "r", "p'", "q'", "r'", "p''", "q''", "r''", ...]
--
-- > > names (undefined :: [Int])
-- > ["xs", "ys", "zs", "xs'", "ys'", "zs'", "xs''", "ys''", ...]
names :: Name a => a -> [String]
names  =  variableNamesFromTemplate . name
