-- |
-- Module      : Data.Haexpress.Name
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Name' type class.
module Data.Haexpress.Name (Name (..)) where

import Data.Maybe (fromJust)
import Data.Either (fromLeft, fromRight)

class Name a where
  name :: a -> String
  name _ = "x"

instance Name ()        where  name _  =  "u"
instance Name Bool      where  name _  =  "p"
instance Name Int       where  name _  =  "x"
instance Name Integer   where  name _  =  "x"
instance Name Char      where  name _  =  "c"
instance Name Ordering  where  name _  =  "o"

instance Name a => Name (Maybe a) where
  name mx  =  "m" ++ name (fromJust mx)

instance (Name a, Name b) => Name (Either a b) where
  name exy  =  "e" ++ name (fromLeft undefined exy)
                   ++ name (fromRight undefined exy)

instance (Name a, Name b, Name c) => Name (a,b,c) where
  name xyz  =  name x ++ name y ++ name z  where  (x,y,z)  =  xyz

instance (Name a, Name b, Name c, Name d) => Name (a,b,c,d) where
  name xyzw  =  name x ++ name y ++ name z ++ name w  where  (x,y,z,w)  =  xyzw

instance Name a => Name [a] where
  name xs  =  name (head xs) ++ "s"
