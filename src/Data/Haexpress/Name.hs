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
  , defNames
  , namesFromTemplate
  )
where

import Data.Char
import Data.List
import Data.Maybe (fromJust)
import Data.Either (fromLeft, fromRight)
import Data.Ratio (Ratio)

class Name a where
  name :: a -> String
  name _ = "x"

instance Name ()        where  name _  =  "u"
instance Name Bool      where  name _  =  "p"
instance Name Int       where  name _  =  "x"
instance Name Integer   where  name _  =  "x"
instance Name Char      where  name _  =  "c"
instance Name Ordering  where  name _  =  "o"

instance Name (Ratio a) where  name _  =  "q"
instance Name Float     where  name _  =  "f"
instance Name Double    where  name _  =  "f"

instance Name a => Name (Maybe a) where
  name mx  =  "m" ++ name (fromJust mx)

instance (Name a, Name b) => Name (Either a b) where
  name exy  =  "e" ++ n ++ m
    where
    x = fromLeft undefined exy
    y = fromRight undefined exy
    n = name x
    m = head $ names y \\ [n]

instance (Name a, Name b) => Name (a,b) where
  name xy  =  n ++ m
    where
    (x,y)  =  xy
    n  =  name x
    m  =  head $ names y \\ [n]

instance (Name a, Name b, Name c) => Name (a,b,c) where
  name xyz  =  n ++ m ++ o
    where
    (x,y,z)  =  xyz
    n  =  name x
    m  =  head $ names y \\ [n]
    o  =  head $ names z \\ [n,m]

instance (Name a, Name b, Name c, Name d) => Name (a,b,c,d) where
  name xyzw  =  name x ++ name y ++ name z ++ name w  where  (x,y,z,w)  =  xyzw

instance Name a => Name [a] where
  name xs  =  name (head xs) ++ "s"

primeCycle :: [String] -> [String]
primeCycle []  =  []
primeCycle ss  =  ss ++ map (++ "'") (primeCycle ss)

namesFromTemplate :: String -> [String]
namesFromTemplate  =  primeCycle . f
  where
  f ""                          =  f "x"
  f cs    | isDigit (last cs)   =  map (\n -> init cs ++ show n) [digitToInt (last cs)..]
  f [c]                         =  map ((:[]) . chr) [x,x+1,x+2] where x = ord c
  f cs    | last cs == 's'      =  (++ "s") <$> f (init cs)
  f "xy"                        =  ["xy","zw"]
  f [c,d] | ord d - ord c == 1  =  [[c,d], [chr $ ord c + 2, chr $ ord d + 2]]
  f cs                          =  [cs]

names :: Name a => a -> [String]
names  =  namesFromTemplate . name

defNames :: [String]
defNames  =  namesFromTemplate "x"
