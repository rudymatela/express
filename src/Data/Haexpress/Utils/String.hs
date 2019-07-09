-- |
-- Module      : Data.Haexpress.Utils.String
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Utilities for manipulating strings.
--
-- At some point, this file was part of the Speculate tool.
module Data.Haexpress.Utils.String
  ( module Data.String
  , module Data.Char
  , unquote
  , atomic
  , outernmostPrec
  , isNegativeLiteral
  , isInfix, isPrefix, isInfixedPrefix
  , toPrefix
  , prec
  , variableNamesFromTemplate
  , primeCycle
  )
where

import Data.String
import Data.Char
import Data.Functor ((<$>)) -- for GHC < 7.10

-- | Unquotes a string if possible, otherwise, this is just an identity.
--
-- > > unquote "\"string\""
-- > "string"
-- > > unquote "something else"
-- > "something else"
unquote :: String -> String
unquote ('"':s) | last s == '"' = init s
unquote s = s

-- | Checks if a string-encoded Haskell expression is atomic.
--
-- > > atomic "123"
-- > True
-- > > atomic "42 + 1337"
-- > False
-- > > atomic "'a'"
-- > True
-- > > atomic "[1,2,3,4,5]"
-- > True
-- > > atomic "(1,2,3,4,5)"
-- > True
--
-- FIXME: The current implementation may produce false positives:
--
-- > > atomic "'a' < 'b'"
-- > True
-- > > atomic "\"asdf\" ++ \"qwer\""
-- > True
-- > > atomic "[1,2,3] ++ [4,5,6]"
-- > True
--
-- but this does not cause problems for (all?) most cases.
atomic :: String -> Bool
atomic s | all (not . isSpace) s = True
atomic ('\'':s) | last s == '\'' = True
atomic ('"':s)  | last s == '"'  = True
atomic ('[':s)  | last s == ']'  = True
atomic ('(':s)  | last s == ')'  = True
atomic _ = False

outernmostPrec :: String -> Maybe Int
outernmostPrec s =
  case words s of
    [l,o,r] | isInfix o -> Just (prec o)
    _                   -> Nothing

isNegativeLiteral :: String -> Bool
isNegativeLiteral s | not (atomic s) = False
isNegativeLiteral "-"                = False
isNegativeLiteral ('-':cs)           = all isDigit cs
isNegativeLiteral _                  = False

-- | Check if a function / operator is infix
--
-- > isInfix "foo"   == False
-- > isInfix "(+)"   == False
-- > isInfix "`foo`" == True
-- > isInfix "+"     == True
isInfix :: String -> Bool
isInfix (c:_) = c `notElem` "()'\"[_" && not (isAlphaNum c)

-- | Returns the precedence of default Haskell operators
prec :: String -> Int
prec " "  = 10
prec "!!" = 9
prec "."  = 9
prec "^"  = 8
prec "^^" = 8
prec "**" = 8
prec "*"  = 7
prec "/"  = 7
prec "%"  = 7
prec "+"  = 6
prec "-"  = 6
prec ":"  = 5
prec "++" = 5
prec "\\" = 5
prec ">"  = 4
prec "<"  = 4
prec ">=" = 4
prec "<=" = 4
prec "==" = 4
prec "/=" = 4
prec "`elem`" = 4
prec "&&" = 3
prec "||" = 2
prec ">>=" = 1
prec ">>" = 1
prec ">=>" = 1
prec "<=<" = 1
prec "$"  = 0
prec "`seq`" = 0
prec "==>" = 0
prec "<==>" = 0
prec _ = 9

isPrefix :: String -> Bool
isPrefix = not . isInfix

-- | Is the string of the form @\`string\`@
isInfixedPrefix :: String -> Bool
isInfixedPrefix s | not (atomic s) = False
isInfixedPrefix ('`':cs)           = last cs == '`'
isInfixedPrefix _                  = False

-- | Transform an infix operator into an infix function:
--
-- > toPrefix "`foo`" == "foo"
-- > toPrefix "+"     == "(+)"
toPrefix :: String -> String
toPrefix ('`':cs) = init cs
toPrefix cs = '(':cs ++ ")"

primeCycle :: [String] -> [String]
primeCycle []  =  []
primeCycle ss  =  ss ++ map (++ "'") (primeCycle ss)

variableNamesFromTemplate :: String -> [String]
variableNamesFromTemplate  =  primeCycle . f
  where
  f ""                           =  f "x"
  f "x"                          =  ["x", "y", "z"] -- redundant, for clarity
  f "xy"                         =  ["xy", "zw"]
  f "xyz"                        =  ["xyz", "uvw"]
  f cs    | isDigit (last cs)    =  map (\n -> init cs ++ show n) [digitToInt (last cs)..]
  f [c]   | c `elem` ['a'..'x']  =  let x = ord c in map ((:[]) . chr) [x,x+1,x+2]
  f cs    | last cs == 's'       =  (++ "s") <$> f (init cs)
  f [c,d] | ord d - ord c == 1   =  [[c,d], [chr $ ord c + 2, chr $ ord d + 2]]
  f cs                           =  cs : map (\n -> cs ++ show n) [1..]
