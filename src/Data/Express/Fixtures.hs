-- |
-- Module      : Data.Express.Fixtures
-- Copyright   : (c) 2019-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines some 'Expr' fixtures to facilitate testing and playing around on
-- the REPL (GHCI).
--
-- * Instead of having to write:
--
--     > > value "&&" (&&) :$ (value "not" not :$ val True) :$ val False
--     > not True && False :: Bool
--
--     Using this module, we can just write:
--
--     > > not' true -&&- false
--     > not True && False :: Bool
--
-- * Instead of having to write:
--
--     > > value "+" ((+)::Int->Int->Int) :$ (value "*" ((*)::Int->Int->Int) :$ var "x" (undefined::Int) :$ var "y" (undefined::Int)) :$ (value "*" ((*)::Int->Int->Int) :$ val (1::Int) :$ val (2::Int))
--     > x * y + 1 * 2 :: Int
--
--     Using this module, we can just write:
--
--     > > xx -*- yy -+- one -*- two
--     > x * y + 1 * 2 :: Int
--
-- * Instead of having to write:
--
--     > > value "||" (||) :$ (value "==" ((==)::Int->Int->Bool) :$ val (3::Int) :$ (value "+" ((+)::Int->Int->Int) :$ var "y" (undefined::Int) :$ val (1::Int))) :$ (value "not" not :$ val False)
--     > 3 == y + 1 || not False :: Bool
--
--     We can just write:
--
--     > > (three -==- yy -+- one) -||- not' false
--     > x == y + 1 || not False :: Bool
--
-- This exports over a hundred symbols
-- to be used mainly when writing unit tests
-- or playing around on GHCi.
--
-- Since the 'Expr' type only allows monomorphic values,
-- encoded polymorphic values are monomorphized
-- usually to the 'Int' type.
--
-- /Beware:/ lifted 'Expr' functions sometimes work for different types.
-- The current version does not have a rationale for types that are included:
-- you have to either try around on the REPL or look at the source to really know.
module Data.Express.Fixtures
  (
  -- * Convenience re-export
    module Data.Express

  -- * Functions and values encoded as Expr or functions of Exprs
  -- | The naming rules are:
  --
  -- * 'Int's are encoded using their English names,
  --   e.g.: 'zero', 'one', 'two';
  --
  -- * 'Char's are encoded using their English names,
  --   e.g.: 'bee', 'cee', 'dee';
  --
  -- * 0-argument constructors are encoded in lowercase,
  --   e.g.: 'false', 'true', 'nothing', 'just';
  --
  -- * lifted constructors are lowercased,
  --   e.g.: 'just';
  --
  -- * lifted functions are primed
  --   e.g.: 'id'', 'negate'', 'head'';
  --
  -- * lifted operators are surrounded by dashes,
  --   e.g.: '-+-', '-*-', '-&&-', '-||-', '-:-'.
  --
  -- * operators are encoded using their English names,
  --   e.g.: 'plus', 'times', 'cons';
  --
  -- * encoded functions are followed by @E@,
  --   e.g.: 'idE', 'notE', 'absE';
  --
  -- * variables have the first character duplicated,
  --   e.g.: 'xx', 'yy', 'xxs';
  --
  -- * encoded values may have the element type appended,
  --   e.g.: 'idInt', 'idBool', 'justInt', 'nilChar'.
  --
  -- Unqualified polymorphic constructors and functions
  -- have their element types bound to 'Int'.
  --
  -- There are exceptions to the above rules such as:
  -- when a name would conflict with a Prelude function.
  -- (e.g.: 'orE' and 'andE')

  -- ** Booleans
  , b_, pp, qq, rr, pp'
  , false
  , true
  , notE
  , orE
  , andE
  , implies
  , not'
  , (-||-)
  , (-&&-)
  , (-==>-)
  , (-==-)
  , (-/=-)
  , (-<=-)
  , (-<-)
  , compare'
  , if'
  , caseBool
  , caseOrdering

  -- ** Integers
  , i_, xx, yy, zz, xx'
  , ii, jj, kk, ii'
  , ll, mm, nn
  , zero, one, two, three, four, five, six
  , seven, eight, nine, ten, eleven, twelve
  , minusOne, minusTwo
  , idE, negateE, absE, signumE
  , idInt
  , idBool
  , idChar
  , idInts
  , idBools
  , idString
  , id', const', negate', abs', signum'
  , plus, times, minus
  , (-+-), (-*-)
  , divE, modE, quotE, remE
  , div', mod', quot', rem'
  , ff, ffE
  , gg, ggE
  , hh, hhE
  , oo, ooE
  , ff2, ff3
  , question
  , (-?-)
  , (-$-)
  , odd'
  , even'

  -- ** Chars
  , c_, cs_
  , cc, dd, ccs
  , ae, bee, cee, dee, zed, zee
  , space, lineBreak
  , ord'
  , ordE

  -- ** Lists
  , is_
  , xxs
  , yys
  , zzs
  , nil
  , emptyString
  , nilInt
  , nilBool
  , nilChar
  , cons
  , consInt
  , consBool
  , consChar
  , (-:-)
  , unit
  , (-++-)
  , head'
  , tail'
  , null'
  , length'
  , init'
  , elem'
  , sort'
  , insert'
  , bs_, pps, qqs
  , and', or'
  , sum', product'
  , appendInt

  -- ** Maybes
  , nothing
  , nothingInt
  , nothingBool
  , just
  , justInt
  , justBool

  -- ** Tuples
  , comma
  , pair
  , (-|-)
  , triple
  , quadruple
  , quintuple
  , sixtuple

  -- ** Ratios
  , (-%-)

  -- ** Higher order
  , compose
  , mapE
  , (-.-)
  , map'
  , filter'

  -- ** Enum
  , enumFrom',   (-..)
  , enumFromTo', (-..-)
  , enumFromThen', (--..)
  , enumFromThenTo', (--..-)
  )
where

import Data.Express
import Data.Maybe
import Data.Typeable (Typeable, typeOf)
import Data.Char
import Data.List
import Data.Ratio

int :: Int
int  =  undefined

bool :: Bool
bool  =  undefined

char :: Char
char  =  undefined

string :: String
string  =  undefined

-- | 'Expr' representing a hole of 'Bool' type.
--
-- > > b_
-- > _ :: Bool
b_ :: Expr
b_  =  hole bool

-- | 'Expr' representing a variable @p :: `Bool`@.
--
-- > > pp
-- > p :: Bool
pp :: Expr
pp  =  var "p" bool

-- | 'Expr' representing a variable @q :: `Bool`@.
--
-- > > qq
-- > q :: Bool
qq :: Expr
qq  =  var "q" bool

-- | 'Expr' representing a variable @r :: `Bool`@.
--
-- > > rr
-- > r :: Bool
rr :: Expr
rr  =  var "r" bool

-- | 'Expr' representing a variable @p' :: `Bool`@.
--
-- > > pp'
-- > p' :: Bool
pp' :: Expr
pp'  =  var "p'" bool


-- | 'False' encoded as an 'Expr'.
--
-- > > false
-- > False :: Bool
false :: Expr
false  =  val False

-- | 'True' encoded as an 'Expr'.
--
-- > > true
-- > True :: Bool
true :: Expr
true  =  val True

-- | The function 'not' encoded as an 'Expr'.
--
-- > > notE
-- > not :: Bool -> Bool
notE :: Expr
notE  =  value "not" not

-- | The function 'and' encoded as an 'Expr'.
--
-- > > andE
-- > (&&) :: Bool -> Bool -> Bool
andE :: Expr
andE  =  value "&&" (&&)

-- | The function 'or' encoded as an 'Expr'.
--
-- > > orE
-- > (||) :: Bool -> Bool -> Bool
orE :: Expr
orE  =  value "||" (||)

-- | The function @==>@ lifted over 'Expr's.
--
-- > > false -==>- true
-- > False ==> True :: Bool
--
-- > > evl $ false -==>- true :: Bool
-- > True
(-==>-) :: Expr -> Expr -> Expr
e1 -==>- e2  =  implies :$ e1 :$ e2
infixr 0 -==>-

-- | The @==>@ operator encoded as an 'Expr'
implies :: Expr
implies  =  value "==>" (==>)
  where
  False ==> _  =  True
  True  ==> p  =  p

-- | The function 'not' lifted over the 'Expr' type.
--
-- > > not' false
-- > not False :: Bool
--
-- > > evalBool $ not' false
-- > True
--
-- > > not' pp
-- > not p :: Bool
not' :: Expr -> Expr
not' pp  =  notE :$ pp

-- | The function '&&' lifted over the 'Expr' type.
--
-- > > pp -&&- qq
-- > p && q :: Bool
--
-- > > false -&&- true
-- > False && True :: Bool
--
-- > > evalBool $ false -&&- true
-- > False
(-&&-) :: Expr -> Expr -> Expr
pp -&&- qq  =  andE :$ pp :$ qq
infixr 3 -&&-

-- | The function '||' lifted over the 'Expr' type.
--
-- > > pp -||- qq
-- > p || q :: Bool
--
-- > > false -||- true
-- > False || True :: Bool
--
-- > > evalBool $ false -||- true
-- > True
(-||-) :: Expr -> Expr -> Expr
pp -||- qq  =  orE :$ pp :$ qq
infixr 2 -||-

-- | A typed hole of 'Int' type.
--
-- > > i_
-- > _ :: Int
i_ :: Expr
i_  =  hole int

-- | A variable @x@ of 'Int' type.
--
-- > > xx
-- > x :: Int
xx :: Expr
xx  =  var "x" int

-- | A variable @y@ of 'Int' type.
--
-- > > yy
-- > y :: Int
yy :: Expr
yy  =  var "y" int

-- | A variable @z@ of 'Int' type.
--
-- > > zz
-- > z :: Int
zz :: Expr
zz  =  var "z" int

-- | A variable @x'@ of 'Int' type.
--
-- > > xx'
-- > x' :: Int
xx' :: Expr
xx'  =  var "x'" int


-- | A variable @i@ of 'Int' type.
--
-- > > ii
-- > i :: Int
ii :: Expr
ii  =  var "i" int

-- | A variable @j@ of 'Int' type.
--
-- > > jj
-- > j :: Int
jj :: Expr
jj  =  var "j" int

-- | A variable @k@ of 'Int' type.
--
-- > > kk
-- > k :: Int
kk :: Expr
kk  =  var "k" int

-- | A variable @i'@ of 'Int' type.
--
-- > > ii'
-- > i' :: Int
ii' :: Expr
ii'  =  var "i'" int

-- | A variable @l@ of 'Int' type.
--
-- > > ll
-- > l :: Int
ll :: Expr
ll  =  var "l" int

-- | A variable @m@ of 'Int' type.
--
-- > > mm
-- > m :: Int
mm :: Expr
mm  =  var "m" int

-- | A variable @n@ of 'Int' type.
--
-- > > nn
-- > n :: Int
nn :: Expr
nn  =  var "n" int

-- | The value @0@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > zero
-- > 0 :: Int
zero :: Expr
zero  =  val (0 :: Int)

-- | The value @1@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > one
-- > 1 :: Int
one :: Expr
one  =  val (1 :: Int)

-- | The value @2@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > two
-- > 2 :: Int
two :: Expr
two  =  val (2 :: Int)

-- | The value @3@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > three
-- > 3 :: Int
three :: Expr
three  =  val (3 :: Int)

-- | The value @4@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > four
-- > 4 :: Int
four :: Expr
four  =  val (4 :: Int)

-- | The value @5@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > five
-- > 5 :: Int
five :: Expr
five  =  val (5 :: Int)

-- | The value @6@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > six
-- > 6 :: Int
six :: Expr
six  =  val (6 :: Int)

-- | The value @7@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > seven
-- > 7 :: Int
seven :: Expr
seven  =  val (7 :: Int)

-- | The value @8@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > eight
-- > 8 :: Int
eight :: Expr
eight  =  val (8 :: Int)

-- | The value @9@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > nine
-- > 9 :: Int
nine :: Expr
nine  =  val (9 :: Int)

-- | The value @10@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > ten
-- > 10 :: Int
ten :: Expr
ten  =  val (10 :: Int)

-- | The value @11@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > eleven
-- > 11 :: Int
eleven :: Expr
eleven  =  val (11 :: Int)

-- | The value @12@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > twelve
-- > 12 :: Int
twelve :: Expr
twelve  =  val (12 :: Int)

-- | The value @-1@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > minusOne
-- > -1 :: Int
minusOne :: Expr
minusOne  =  val (-1 :: Int)

-- | The value @-2@ bound to the 'Int' type encoded as an 'Expr'.
--
-- > > minusOne
-- > -2 :: Int
minusTwo :: Expr
minusTwo  =  val (-2 :: Int)

-- | A variable function @f@ of 'a -> a' type lifted over the 'Expr' type.
--   This works for 'Int', 'Bool', 'Char' and their lists
--
-- > > ff xx
-- > f x :: Int
--
-- > > ff one
-- > f 1 :: Int
ff :: Expr -> Expr
ff  =  mk1 "ff"
  [ var "f" (undefined :: F Int)
  , var "f" (undefined :: F Bool)
  , var "f" (undefined :: F Char)
  , var "f" (undefined :: F [Int])
  , var "f" (undefined :: F [Bool])
  , var "f" (undefined :: F String)
  ]
type F a  =  a -> a

-- | A variable @f@ of 'Int -> Int' type encoded as an 'Expr'.
--
-- > > ffE
-- > f :: Int -> Int
ffE :: Expr
ffE  =  var "f" (undefined :: Int -> Int)

-- | A variable function @g@ of 'a -> a' type lifted over the 'Expr' type.
--   This works for 'Int', 'Bool', 'Char' and their lists.
--
-- > > gg yy
-- > g y :: Int
--
-- > > gg minusTwo
-- > gg (-2) :: Int
gg :: Expr -> Expr
gg  =  mk1 "gg"
  [ var "g" (undefined :: F Int)
  , var "g" (undefined :: F Bool)
  , var "g" (undefined :: F Char)
  , var "g" (undefined :: F [Int])
  , var "g" (undefined :: F [Bool])
  , var "g" (undefined :: F String)
  ]

-- | A variable @g@ of 'Int -> Int' type encoded as an 'Expr'.
--
-- > > ggE
-- > g :: Int -> Int
ggE :: Expr
ggE  =  var "g" (undefined :: Int -> Int)

-- | A variable function @h@ of 'Int -> Int' type lifted over the 'Expr' type.
--
-- > > hh zz
-- > h z :: Int
hh :: Expr -> Expr
hh  =  mk1 "hh"
  [ var "h" (undefined :: F Int)
  , var "h" (undefined :: F Bool)
  , var "h" (undefined :: F Char)
  , var "h" (undefined :: F [Int])
  , var "h" (undefined :: F [Bool])
  , var "h" (undefined :: F String)
  ]

-- | A variable @h@ of 'Int -> Int' type encoded as an 'Expr'.
--
-- > > hhE
-- > h :: Int -> Int
hhE :: Expr
hhE  =  var "h" (undefined :: Int -> Int)

-- | A variable function @f@ of 'a -> a -> a' type lifted over the 'Expr' type.
--   This works for 'Int', 'Bool', 'Char' and their lists
--
-- > > ff2 xx yy
-- > f x y :: Int
--
-- > > ff2 one two
-- > f 1 2 :: Int
--
-- The result type is bound to the type of the last argument.
ff2 :: Expr -> Expr -> Expr
ff2  =  mk2 "ff2"
  [ var "f" (undefined :: F21 Int)
  , var "f" (undefined :: F21 Bool)
  , var "f" (undefined :: F21 Char)
  , var "f" (undefined :: F21 [Int])
  , var "f" (undefined :: F21 [Bool])
  , var "f" (undefined :: F21 String)
  , var "f" (undefined :: F2 Int [Int])
  , var "f" (undefined :: F2 Bool [Bool])
  , var "f" (undefined :: F2 Char [Char])
  , var "f" (undefined :: F2 [Int] Int)
  , var "f" (undefined :: F2 [Bool] Bool)
  , var "f" (undefined :: F2 [Char] Char)
  , var "f" (undefined :: F2 Int Bool)
  , var "f" (undefined :: F2 Bool Int)
  ]
type F2 a b  =  a -> b -> b
type F21 a  =  F2 a a

-- | A variable function @f@ of 'a -> a -> a -> a' type lifted over the 'Expr' type.
--   This works for 'Int', 'Bool', 'Char' and their lists
--
-- > > ff3 xx yy zz
-- > f x y z :: Int
--
-- > > ff3 one two three
-- > f 1 2 3 :: Int
--
-- The result type is bound to the type of the last argument.
ff3 :: Expr -> Expr -> Expr -> Expr
ff3  =  mk3 "ff3"
  [ var "f" (undefined :: F31 Int)
  , var "f" (undefined :: F31 Bool)
  , var "f" (undefined :: F31 Char)
  , var "f" (undefined :: F31 [Int])
  , var "f" (undefined :: F31 [Bool])
  , var "f" (undefined :: F31 String)
  , var "f" (undefined :: F3 Int Int [Int])
  , var "f" (undefined :: F3 Int [Int] Int)
  , var "f" (undefined :: F3 [Int] Int Int)
  , var "f" (undefined :: F3 Int [Int] [Int])
  , var "f" (undefined :: F3 [Int] Int [Int])
  , var "f" (undefined :: F3 [Int] [Int] Int)
  ]
type F3 a b c  =  a -> b -> c -> c
type F31 a  =  F3 a a a

-- | A variable binary operator @?@ lifted over the 'Expr' type.
--   Works for 'Int', 'Bool', 'Char', @[Int]@ and 'String'.
--
-- > > xx -?- yy
-- > x ? y :: Int
--
-- > > pp -?- qq
-- > p ? q :: Bool
--
-- > > xx -?- qq
-- > *** Exception: (-?-): unhandled type: 1 :: Int, False :: Bool
-- >     accepted types are:
-- >     (?) :: Int -> Int -> Int
-- >     (?) :: Bool -> Bool -> Bool
-- >     (?) :: Char -> Char -> Char
-- >     (?) :: [Int] -> [Int] -> [Int]
-- >     (?) :: [Char] -> [Char] -> [Char]
-- >     (?) :: Int -> [Int] -> [Int]
-- >     (?) :: Char -> [Char] -> [Char]
(-?-) :: Expr -> Expr -> Expr
(-?-)  =  mk2 "(-?-)"
  [ var "?" (undefined :: Int -> Int -> Int)
  , var "?" (undefined :: Bool -> Bool -> Bool)
  , var "?" (undefined :: Char -> Char -> Char)
  , var "?" (undefined :: [Int] -> [Int] -> [Int])
  , var "?" (undefined :: String -> String -> String)
  , var "?" (undefined :: Int -> [Int] -> [Int])
  , var "?" (undefined :: Char -> String -> String)
  ]

-- | A variable binary operator @?@ encoded as an 'Expr' (cf. '-?-')
--
-- > > question :$ xx :$ yy
-- > x ? y :: Int
--
-- > > question :$ pp :$ qq
-- > p ? q :: Bool
question :: Expr
question  =  var "?" (undefined :: Int -> Int -> Int)

-- | A variable binary operator @o@ lifted over the 'Expr' type.
--   Works for 'Int', 'Bool', 'Char', @[Int]@ and 'String'.
--
-- > > xx `oo` yy
-- > x `o` y :: Int
--
-- > > pp `oo` qq
-- > p `o` q :: Bool
--
-- > > xx `oo` qq
-- > *** Exception: oo: unhandled type: 1 :: Int, False :: Bool
-- >     accepted types are:
-- >     o :: Int -> Int -> Int
-- >     o :: Bool -> Bool -> Bool
-- >     o :: Char -> Char -> Char
-- >     o :: [Int] -> [Int] -> [Int]
-- >     o :: [Char] -> [Char] -> [Char]
oo :: Expr -> Expr -> Expr
oo  =  mk2 "oo"
  [ var "`o`" (undefined :: Int -> Int -> Int)
  , var "`o`" (undefined :: Bool -> Bool -> Bool)
  , var "`o`" (undefined :: Char -> Char -> Char)
  , var "`o`" (undefined :: [Int] -> [Int] -> [Int])
  , var "`o`" (undefined :: String -> String -> String)
  ]

-- | A variable binary function @o@ encoded as an 'Expr' (cf. 'oo')
--
-- > > ooE :$ xx :$ yy
-- > x `o` y :: Int
--
-- > > ooE :$ pp :$ qq
-- > p `o` q :: Bool
ooE :: Expr
ooE  =  var "`o`" (undefined :: Int -> Int -> Int)

-- | The operator '+' for the 'Int' type for use on 'Expr's.  (See also 'plus'.)
--
-- > > two -+- three
-- > 2 + 3 :: Int
--
-- > > minusOne -+- minusTwo -+- zero
-- > ((-1) + (-2)) + 0 :: Int
--
-- > > xx -+- (yy -+- zz)
-- > x + (y + z) :: Int
(-+-) :: Expr -> Expr -> Expr
e1 -+- e2  =  plus :$ e1 :$ e2
infixl 6 -+-

-- | The operator '+' for the 'Int' type.  (See also '-+-'.)
--
-- > > plus
-- > (+) :: Int -> Int -> Int
--
-- > > plus :$ one
-- > (1 +) :: Int -> Int
--
-- > > plus :$ xx :$ yy
-- > x + y :: Int
plus :: Expr
plus  =  value "+" ((+) :: Int -> Int -> Int)

-- | The operator '*' for the 'Int' type lifted over the 'Expr' type.  (See also 'times'.)
--
-- > > three -*- three
-- > 3 * 3 :: Int
--
-- > > one -*- two -*- three
-- > (1 * 2) * 3 :: Int
--
-- > > two -*- xx
-- > 2 * x :: Int
(-*-) :: Expr -> Expr -> Expr
e1 -*- e2  =  times :$ e1 :$ e2
infixl 7 -*-

-- | The operator '*' for the 'Int' type.  (See also '-*-'.)
--
-- > > times
-- > (*) :: Int -> Int -> Int
--
-- > > times :$ two
-- > (2 *) :: Int -> Int
--
-- > > times :$ xx :$ yy
-- > x * y :: Int
times :: Expr
times  =  value "*" ((*) :: Int -> Int -> Int)

-- | The subtraction '-' operator encoded as an 'Expr'.
--
-- > > minus :$ one
-- > (1 -) :: Int -> Int
--
-- > > minus :$ one :$ zero
-- > 1 - 0 :: Int
minus :: Expr
minus  =  value "-" ((-) :: Int -> Int -> Int)

-- | The function 'div' for the 'Int' type lifted over the 'Expr' type.  (See also 'mod'.)
--
-- > > six `div'` four
-- > 6 `div` 4 :: Int
div' :: Expr -> Expr -> Expr
ex `div'` ey  =  divE :$ ex :$ ey

-- | Integer division 'div' encoded as an 'Expr'.
--
-- > > divE :$ two
-- > (2 `div`) :: Int -> Int
--
-- > > divE :$ two :$ three
-- > 2 `div` 3 :: Int
divE :: Expr
divE  =  value "`div`" (div :: Int -> Int -> Int)

-- | The function 'mod' for the 'Int' type lifted over the 'Expr' type.  (See also 'div'.)
--
-- > > six `mod'` four
-- > 6 `mod` 4 :: Int
mod' :: Expr -> Expr -> Expr
ex `mod'` ey  =  modE :$ ex :$ ey

-- | Integer modulo 'mod' encoded as an 'Expr'.
--
-- > > modE :$ two
-- > (2 `mod`) :: Int -> Int
--
-- > > modE :$ two :$ three
-- > 2 `mod` 3 :: Int
modE :: Expr
modE  =  value "`mod`" (mod :: Int -> Int -> Int)

-- | The function 'quot' for the 'Int' type lifted over the 'Expr' type.  (See also 'rem'.)
--
-- > > six `quot'` four
-- > 6 `quot` 4 :: Int
quot' :: Expr -> Expr -> Expr
ex `quot'` ey  =  quotE :$ ex :$ ey

-- | Integer quotient 'quot' encoded as an 'Expr'.
--
-- > > quotE :$ two
-- > (2 `quot`) :: Int -> Int
--
-- > > quotE :$ two :$ three
-- > 2 `quot` 3 :: Int
quotE :: Expr
quotE  =  value "`quot`" (quot :: Int -> Int -> Int)

-- | The function 'rem' for the 'Int' type lifted over the 'Expr' type.  (See also 'quot'.)
--
-- > > six `rem'` four
-- > 6 `rem` 4 :: Int
rem' :: Expr -> Expr -> Expr
ex `rem'` ey  =  remE :$ ex :$ ey

-- | Integer remainder 'rem' encoded as an 'Expr'.
--
-- > > remE :$ two
-- > (2 `rem`) :: Int -> Int
--
-- > > remE :$ two :$ three
-- > 2 `rem` 3 :: Int
remE :: Expr
remE  =  value "`rem`" (rem :: Int -> Int -> Int)

-- | Constructs an application of 'id' as an 'Expr'.
--   Only works for 'Int', 'Bool', 'Char', 'String', @[Int]@, @[Bool]@.
--
-- > > id' yy
-- > id yy :: Int
--
-- > > id' one
-- > id 1 :: Int
--
-- > > evl (id' one) :: Int
-- > 1
--
-- > > id' pp
-- > id p :: Bool
--
-- > > id' false
-- > id' False :: Bool
--
-- > > evl (id' true) :: Bool
-- > True :: Bool
id' :: Expr -> Expr
id'  =  mk1 "id'"
  [ idInt
  , idBool
  , idChar
  , idInts
  , idBools
  , idString
  ]

-- | The function 'id' for the 'Int' type encoded as an 'Expr'.  (See also 'id''.)
--
-- > > idE :$ xx
-- > id x :: Int
--
-- > > idE :$ zero
-- > id 0 :: Int
--
-- > > evaluate $ idE :$ zero :: Maybe Int
-- > Just 0
idE :: Expr
idE  =  idInt

-- | The function 'id' encoded as an 'Expr'.  (cf. 'id'')
idInt,idBool,idChar,idInts,idBools,idString :: Expr
idInt     =  value "id" (id :: Id Int)
idBool    =  value "id" (id :: Id Bool)
idChar    =  value "id" (id :: Id Char)
idInts    =  value "id" (id :: Id [Int])
idBools   =  value "id" (id :: Id [Bool])
idString  =  value "id" (id :: Id String)
type Id a  =  a -> a

-- | The 'const' function lifted over the 'Expr' type.
--
-- > > const' zero one
-- > const 0 1 :: Int
--
-- This works for the argument types 'Int', 'Char', 'Bool' and their lists.
const' :: Expr -> Expr -> Expr
const'  =  mk2 "const'"
  [ value "const" (const :: Int -> Int -> Int)
  , value "const" (const :: Bool -> Bool -> Bool)
  , value "const" (const :: Char -> Char -> Char)
  , value "const" (const :: [Int] -> [Int] -> [Int])
  , value "const" (const :: [Bool] -> [Bool] -> [Bool])
  , value "const" (const :: String -> String -> String)
  ]

-- | 'negate' over the 'Int' type lifted over the 'Expr' type.
--
-- > > negate' xx
-- > negate x :: Int
--
-- > > evl (negate' one) :: Int
-- > -1
negate' :: Expr -> Expr
negate' e  =  negateE :$ e

-- | 'negate' over the 'Int' type encoded as an 'Expr'
--
-- > > negateE
-- > negate :: Int -> Int
negateE :: Expr
negateE  =  value "negate" (negate :: Int -> Int)

-- | 'abs' over the 'Int' type lifted over the 'Expr' type.
--
-- > > abs' xx'
-- > abs x' :: Int
--
-- > > evl (abs' minusTwo) :: Int
-- > 2
abs' :: Expr -> Expr
abs' e  =  absE :$ e

-- | 'abs' over the 'Int' type encoded as an 'Expr'.
--
-- > > absE
-- > abs :: Int -> Int
absE :: Expr
absE  =  value "abs" (abs :: Int -> Int)

-- | 'signum' over the 'Int' type lifted over the 'Expr' type.
--
-- > > signum' xx'
-- > signum x' :: Int
--
-- > > evl (signum' minusTwo) :: Int
-- > -1
signum' :: Expr -> Expr
signum' e  =  signumE :$ e

-- | 'signum' over the 'Int' type encoded as an 'Expr'.
--
-- > > signumE
-- > signum :: Int -> Int
signumE :: Expr
signumE  =  value "signum" (signum :: Int -> Int)

-- | 'odd' with an 'Int' argument lifted over the 'Expr' type.
--
-- > > odd' (xx -+- one)
-- > odd (x + 1) :: Bool
--
-- > > evl (odd' two) :: Bool
-- > False
odd' :: Expr -> Expr
odd'  =  (oddE :$) where oddE = value "odd" (odd :: Int -> Bool)

-- | 'even' with an 'Int' argument lifted over the 'Expr' type.
--
-- > > even' (xx -+- two)
-- > even (x + 2) :: Bool
--
-- > > evl (even' two) :: Bool
-- > True
even' :: Expr -> Expr
even'  =  (evenE :$) where evenE = value "even" (even :: Int -> Bool)

-- | A hole of 'Char' type encoded as an 'Expr'.
--
-- > > c_
-- > _ :: Char
c_ :: Expr
c_  =  hole char

-- | A hole of 'String' type encoded as an 'Expr'.
--
-- > > cs_
-- > _ :: [Char]
cs_ :: Expr
cs_  =  hole [char]

-- | A variable named @c@ of type 'Char' encoded as an 'Expr'.
--
-- > > cc
-- > c :: Char
cc :: Expr
cc  =  var "c" char

-- | A variable named @c@ of type 'Char' encoded as an 'Expr'.
--
-- > > dd
-- > d :: Char
dd :: Expr
dd  =  var "d" char

-- | A variable named @cs@ of type 'String' encoded as an 'Expr'.
--
-- > > ccs
-- > cs :: [Char]
ccs :: Expr
ccs  =  var "cs" [char]

-- | The character @\'a\'@ encoded as an 'Expr'.
--
-- > > ae
-- > 'a' :: Char
--
-- > > evl ae :: Char
-- > 'a'
ae :: Expr
ae  =  val 'a'
-- The English name for letter 'a' is not really 'ae', but simply 'a'.

-- | The character @\'b\'@ encoded as an 'Expr'
--
-- > > bee
-- > 'b' :: Char
--
-- > > evl bee :: Char
-- > 'b'
bee :: Expr
bee  =  val 'b'

-- | The character @\'c\'@ encoded as an 'Expr'
--
-- > > cee
-- > 'c' :: Char
--
-- > > evl cee :: Char
-- > 'c'
cee :: Expr
cee  =  val 'c'

-- | The character @\'d\'@ encoded as an 'Expr'
--
-- > > dee
-- > 'd' :: Char
--
-- > > evl dee :: Char
-- > 'd'
dee :: Expr
dee  =  val 'd'

-- | The character @\'z\'@ encoded as an 'Expr'
--
-- > > zed
-- > 'z' :: Char
--
-- > > evl zed :: Char
-- > 'z'
--
-- (cf. 'zee')
zed :: Expr
zed  =  val 'z'

-- | The character @\'z\'@ encoded as an 'Expr'
--
-- > > zee
-- > 'z' :: Char
--
-- > > evl zee :: Char
-- > 'z'
--
-- (cf. 'zed')
zee :: Expr
zee  =  val 'z'

-- | The space character encoded as an 'Expr'
--
-- > > space
-- > ' ' :: Char
space :: Expr
space  =  val ' '

-- | The line break character encoded as an 'Expr'
--
-- > > lineBreak
-- > '\n' :: Char
lineBreak :: Expr
lineBreak  =  val '\n'

-- | The 'ord' function lifted over 'Expr'
--
-- > > ord' bee
-- > ord 'b' :: Int
--
-- > > evl (ord' bee)
-- > 98
ord' :: Expr -> Expr
ord'  =  (ordE :$)

-- | The 'ord' function encoded as an 'Expr'
ordE :: Expr
ordE  =  value "ord" ord

-- | A typed hole of @[Int]@ type encoded as an 'Expr'.
--
-- > > is_
-- > _ :: [Int]
is_ :: Expr
is_  =  hole [int]

-- | A variable named @xs@ of type @[Int]@ encoded as an 'Expr'.
--
-- > > xxs
-- > xs :: [Int]
xxs :: Expr
xxs  =  var "xs" [int]

-- | A variable named @ys@ of type @[Int]@ encoded as an 'Expr'.
--
-- > > yys
-- > ys :: [Int]
yys :: Expr
yys  =  var "ys" [int]

-- | A variable named @zs@ of type @[Int]@ encoded as an 'Expr'.
--
-- > > yys
-- > ys :: [Int]
zzs :: Expr
zzs  =  var "zs" [int]

-- | An empty list of type @[Int]@ encoded as an 'Expr'.
--
-- > > nil
-- > [] :: [Int]
nil :: Expr
nil  =  nilInt

-- | An empty 'String' encoded as an 'Expr'.
--
-- > > emptyString
-- > "" :: String
emptyString :: Expr
emptyString  =  val ""

-- | The empty list '[]' encoded as an 'Expr'.
nilInt, nilBool, nilChar :: Expr
nilInt   =  val ([] :: [Int])
nilBool  =  val ([] :: [Bool])
nilChar  =  value "[]" ([] :: [Char])
nilInts  =  val ([] :: [[Int]])

-- | The list constructor with 'Int' as element type encoded as an 'Expr'.
--
-- > > cons
-- > (:) :: Int -> [Int] -> [Int]
--
-- > > cons :$ one :$ nil
-- > [1] :: [Int]
--
-- Consider using '-:-' and 'unit' when building lists of 'Expr'.
cons :: Expr
cons  =  consInt

-- | The list constructor @ : @ encoded as an 'Expr'.
consInt, consBool, consChar :: Expr
consInt   =  value ":" ((:) :: Cons Int)
consBool  =  value ":" ((:) :: Cons Bool)
consChar  =  value ":" ((:) :: Cons Char)
type Cons a  =  a -> [a] -> [a]

-- | 'unit' constructs a list with a single element.
--   This works for elements of type 'Int', 'Char' and 'Bool'.
--
-- > > unit one
-- > [1]
--
-- > > unit false
-- > [False]
unit :: Expr -> Expr
unit e  =  e -:- nil'
  where
  nil' | typ e == typ i_  =  nilInt
       | typ e == typ c_  =  emptyString
       | typ e == typ b_  =  nilBool
       | typ e == typ is_  =  nilInts
       | otherwise  =  err "unit" [e] [nil,emptyString,nilBool,nilInts]

-- | The list constructor lifted over the 'Expr' type.
--   Works for the element types 'Int', 'Char' and 'Bool'.
--
-- > > zero -:- one -:- unit two
-- > [0,1,2] :: [Int]
--
-- > > zero -:- one -:- two -:- nil
-- > [0,1,2] :: [Int]
--
-- > > bee -:- unit cee
-- > "bc" :: [Char]
(-:-) :: Expr -> Expr -> Expr
(-:-)  =  mk2 "(-:-)"
  [ consInt
  , consBool
  , consChar
  , value ":" ((:) :: Cons [Int])
  , value ":" ((:) :: Cons [Bool])
  , value ":" ((:) :: Cons String)
  , value ":" ((:) :: Cons (Int,Int))
  , value ":" ((:) :: Cons (Bool,Bool))
  , value ":" ((:) :: Cons (Char,Char))
  , value ":" ((:) :: Cons (Maybe Int))
  , value ":" ((:) :: Cons (Either Int Int))
  ]
infixr 5 -:-

-- | Append for list of 'Int's encoded as an 'Expr'.
appendInt :: Expr
appendInt  =  value "++" ((++) :: [Int] -> [Int] -> [Int])

-- | List concatenation lifted over the 'Expr' type.
--   Works for the element types 'Int', 'Char' and 'Bool'.
--
-- > > (zero -:- one -:- nil) -:- (two -:- three -:- nil)
-- > [0,1] -++- [2,3] :: [Int]
--
-- > > (bee -:- unit cee) -:- unit dee
-- > "bc" -++- "c" :: [Char]
(-++-) :: Expr -> Expr -> Expr
(-++-)  =  mk2 "(-++-)"
  [ value "++" ((++) :: Append Int)
  , value "++" ((++) :: Append Char)
  , value "++" ((++) :: Append Bool)
  , value "++" ((++) :: Append [Int])
  , value "++" ((++) :: Append [Char])
  , value "++" ((++) :: Append [Bool])
  , value "++" ((++) :: Append (Int,Int))
  , value "++" ((++) :: Append (Maybe Int))
  ]
infixr 5 -++-
type Append a  =  [a] -> [a] -> [a]

-- | List 'head' lifted over the 'Expr' type.
--   Works for the element types 'Int', 'Char' and 'Bool'.
--
-- > > head' $ unit one
-- > head [1] :: Int
--
-- > > head' $ unit bee
-- > head "b" :: Char
--
-- > > head' $ zero -:- unit two
-- > head [0,2] :: Int
--
-- > > evl $ head' $ unit one :: Int
-- > 1
head' :: Expr -> Expr
head'  =  mk1 "head'"
  [ value "head" (head :: Head Int)
  , value "head" (head :: Head Char)
  , value "head" (head :: Head Bool)
  , value "head" (head :: Head [Int])
  , value "head" (head :: Head [Char])
  , value "head" (head :: Head [Bool])
  , value "head" (head :: Head (Int,Int))
  , value "head" (head :: Head (Maybe Int))
  ]
type Head a  =  [a] -> a

-- | List 'tail' lifted over the 'Expr' type.
--   Works for the element types 'Int', 'Char' and 'Bool'.
--
-- > > tail' $ unit one
-- > tail [1] :: [Int]
--
-- > > tail' $ unit bee
-- > tail "b" :: [Char]
--
-- > > tail' $ zero -:- unit two
-- > tail [0,2] :: [Int]
--
-- > > evl $ tail' $ zero -:- unit two :: [Int]
-- > [2]
tail' :: Expr -> Expr
tail'  =  mk1 "tail'"
  [ value "tail" (tail :: Tail Int)
  , value "tail" (tail :: Tail Char)
  , value "tail" (tail :: Tail Bool)
  , value "tail" (tail :: Tail [Int])
  , value "tail" (tail :: Tail [Char])
  , value "tail" (tail :: Tail [Bool])
  , value "tail" (tail :: Tail (Int,Int))
  , value "tail" (tail :: Tail (Maybe Int))
  ]
type Tail a  =  [a] -> [a]

-- | List 'null' lifted over the 'Expr' type.
--   Works for the element types 'Int', 'Char' and 'Bool'.
--
-- > > null' $ unit one
-- > null [1] :: Bool
--
-- > > null' $ nil
-- > null [] :: Bool
--
-- > > evl $ null' nil :: Bool
-- > True
null' :: Expr -> Expr
null'  =  mk1 "null'"
  [ value "null" (null :: Null Int)
  , value "null" (null :: Null Char)
  , value "null" (null :: Null Bool)
  , value "null" (null :: Null [Int])
  , value "null" (null :: Null [Char])
  , value "null" (null :: Null [Bool])
  , value "null" (null :: Null (Int,Int))
  , value "null" (null :: Null (Maybe Int))
  ]
type Null a  =  [a] -> Bool

-- | List 'length' lifted over the 'Expr' type.
--   Works for the element types 'Int', 'Char' and 'Bool'.
--
-- > > length' $ unit one
-- > length [1] :: Int
--
-- > > length' $ unit bee
-- > length "b" :: Int
--
-- > > length' $ zero -:- unit two
-- > length [0,2] :: Int
--
-- > > evl $ length' $ unit one :: Int
-- > 1
length' :: Expr -> Expr
length'  =  mk1 "length'"
  [ value "length" (length :: Length Int)
  , value "length" (length :: Length Char)
  , value "length" (length :: Length Bool)
  , value "length" (length :: Length [Int])
  , value "length" (length :: Length [Char])
  , value "length" (length :: Length [Bool])
  , value "length" (length :: Length (Int,Int))
  , value "length" (length :: Length (Maybe Int))
  ]
type Length a  =  [a] -> Int

-- | List 'init' lifted over the 'Expr' type.
--   Works for the element types 'Int', 'Char' and 'Bool'.
--
-- > > init' $ unit one
-- > init [1] :: [Int]
--
-- > > init' $ unit bee
-- > init "b" :: [Char]
--
-- > > init' $ zero -:- unit two
-- > init [0,2] :: [Int]
--
-- > > evl $ init' $ zero -:- unit two :: [Int]
-- > [0]
init' :: Expr -> Expr
init'  =  mk1 "init'"
  [ value "init" (init :: Init Int)
  , value "init" (init :: Init Char)
  , value "init" (init :: Init Bool)
  , value "init" (init :: Init [Int])
  , value "init" (init :: Init [Char])
  , value "init" (init :: Init [Bool])
  , value "init" (init :: Init (Int,Int))
  , value "init" (init :: Init (Maybe Int))
  ]
type Init a  =  Tail a

-- | List 'sort' lifted over the 'Expr' type.
--   Works for the element types 'Int', 'Char' and 'Bool'.
--
-- > > sort' $ unit one
-- > sort [1] :: Int
--
-- > > sort' $ unit bee
-- > sort "b" :: Int
--
-- > > sort' $ zero -:- unit two
-- > sort [0,2] :: Int
--
-- > > evl $ sort' $ two -:- unit one :: [Int]
-- > [1,2]
sort' :: Expr -> Expr
sort'  =  mk1 "sort'"
  [ value "sort" (sort :: Sort Int)
  , value "sort" (sort :: Sort Char)
  , value "sort" (sort :: Sort Bool)
  , value "sort" (sort :: Sort [Int])
  , value "sort" (sort :: Sort [Char])
  , value "sort" (sort :: Sort [Bool])
  , value "sort" (sort :: Sort (Int,Int))
  , value "sort" (sort :: Sort (Maybe Int))
  ]
type Sort a  =  Tail a

-- | List 'insert' lifted over the 'Expr' type.
--   Works for the element types 'Int', 'Char' and 'Bool'.
--
-- > > insert' zero nilInt
-- > insert 0 [] :: [Int]
--
-- > > insert' false (false -:- unit true)
-- > insert False [False,True] :: [Bool]
insert' :: Expr -> Expr -> Expr
insert'  =  mk2 "insert'"
  [ value "insert" (insert :: Insert Int)
  , value "insert" (insert :: Insert Char)
  , value "insert" (insert :: Insert Bool)
  , value "insert" (insert :: Insert [Int])
  , value "insert" (insert :: Insert [Char])
  , value "insert" (insert :: Insert [Bool])
  , value "insert" (insert :: Insert (Int,Int))
  , value "insert" (insert :: Insert (Maybe Int))
  ]
type Insert a  =  Cons a

-- | List 'elem' lifted over the 'Expr' type.
--   Works for the element types 'Int', 'Char' and 'Bool'.
--
-- > > elem' false (false -:- unit true)
-- > elem False [False,True] :: Bool
--
-- > > evl $ elem' false (false -:- unit true) :: Bool
-- > True
elem' :: Expr -> Expr -> Expr
elem'  =  mk2 "elem'"
  [ value "elem" (elem :: Elem Int)
  , value "elem" (elem :: Elem Char)
  , value "elem" (elem :: Elem Bool)
  , value "elem" (elem :: Elem [Int])
  , value "elem" (elem :: Elem [Char])
  , value "elem" (elem :: Elem [Bool])
  , value "elem" (elem :: Elem (Int,Int))
  , value "elem" (elem :: Elem (Maybe Int))
  ]
type Elem a  =  a -> [a] -> Bool

-- | '$' lifted over 'Expr's
--
-- > > absE -$- one
-- > abs $ 1 :: Int
--
-- Works for 'Int', 'Bool', 'Char' argument types and their lists.
(-$-) :: Expr -> Expr -> Expr
(-$-)  =  mk2 "(-$-)"
  [ value "$" (($) :: Apply Int)
  , value "$" (($) :: Apply Bool)
  , value "$" (($) :: Apply Char)
  , value "$" (($) :: Apply [Int])
  , value "$" (($) :: Apply [Bool])
  , value "$" (($) :: Apply [Char])
  , value "$" (($) :: Apply (Int,Int))
  , value "$" (($) :: Apply (Bool,Bool))
  , value "$" (($) :: Apply (Char,Char))
  , value "$" (($) :: Apply (Maybe Int))
  ]
infixl 6 -$-
type Apply a  =  (a -> a) -> a -> a

-- | Constructs an equation between two 'Expr's.
--
-- > > xx -==- zero
-- > x == 0 :: Bool
--
-- > > cc -==- dee
-- > c == 'd' :: Bool
--
-- This works for the 'Int', 'Bool', 'Char' argument types and their lists.
(-==-) :: Expr -> Expr -> Expr
(-==-)  =  mk2 "(-==-)"
  [ value "==" ((==) :: Comparison ())
  , value "==" ((==) :: Comparison Int)
  , value "==" ((==) :: Comparison Bool)
  , value "==" ((==) :: Comparison Char)
  , value "==" ((==) :: Comparison [Int])
  , value "==" ((==) :: Comparison [Bool])
  , value "==" ((==) :: Comparison [Char])
  , value "==" ((==) :: Comparison [[Int]])
  , value "==" ((==) :: Comparison [[Bool]])
  , value "==" ((==) :: Comparison [[Char]])
  , value "==" ((==) :: Comparison (Int,Int))
  , value "==" ((==) :: Comparison (Bool,Bool))
  , value "==" ((==) :: Comparison (Char,Char))
  , value "==" ((==) :: Comparison (Maybe Int))
  , value "==" ((==) :: Comparison (Int,Int,Int))
  ]
infix 4 -==-
type Comparison a  =  a -> a -> Bool

-- | Constructs an inequation between two 'Expr's.
--
-- > > xx -/=- zero
-- > x /= 0 :: Bool
--
-- > > cc -/=- ae
-- > c /= 'a' :: Bool
(-/=-) :: Expr -> Expr -> Expr
(-/=-)  =  mk2 "(-/=-)"
  [ value "/=" ((/=) :: Comparison ())
  , value "/=" ((/=) :: Comparison Int)
  , value "/=" ((/=) :: Comparison Bool)
  , value "/=" ((/=) :: Comparison Char)
  , value "/=" ((/=) :: Comparison [Int])
  , value "/=" ((/=) :: Comparison [Bool])
  , value "/=" ((/=) :: Comparison [Char])
  , value "/=" ((/=) :: Comparison [[Int]])
  , value "/=" ((/=) :: Comparison [[Bool]])
  , value "/=" ((/=) :: Comparison [[Char]])
  , value "/=" ((/=) :: Comparison (Int,Int))
  , value "/=" ((/=) :: Comparison (Bool,Bool))
  , value "/=" ((/=) :: Comparison (Char,Char))
  , value "/=" ((/=) :: Comparison (Maybe Int))
  , value "/=" ((/=) :: Comparison (Int,Int,Int))
  ]
infix 4 -/=-

-- | Constructs a less-than-or-equal inequation between two 'Expr's.
--
-- > > xx -<=- zero
-- > x <= 0 :: Bool
--
-- > > cc -<=- ae
-- > c <= 'a' :: Bool
(-<=-) :: Expr -> Expr -> Expr
(-<=-)  =  mk2 "(-<=-)"
  [ value "<=" ((<=) :: Comparison ())
  , value "<=" ((<=) :: Comparison Int)
  , value "<=" ((<=) :: Comparison Bool)
  , value "<=" ((<=) :: Comparison Char)
  , value "<=" ((<=) :: Comparison [Int])
  , value "<=" ((<=) :: Comparison [Bool])
  , value "<=" ((<=) :: Comparison [Char])
  , value "<=" ((<=) :: Comparison [[Int]])
  , value "<=" ((<=) :: Comparison [[Bool]])
  , value "<=" ((<=) :: Comparison [[Char]])
  , value "<=" ((<=) :: Comparison (Int,Int))
  , value "<=" ((<=) :: Comparison (Bool,Bool))
  , value "<=" ((<=) :: Comparison (Char,Char))
  , value "<=" ((<=) :: Comparison (Maybe Int))
  , value "<=" ((<=) :: Comparison (Int,Int,Int))
  ]
infix 4 -<=-

-- | Constructs a less-than inequation between two 'Expr's.
--
-- > > xx -<- zero
-- > x < 0 :: Bool
--
-- > > cc -<- bee
-- > c < 'b' :: Bool
(-<-) :: Expr -> Expr -> Expr
(-<-)  =  mk2 "(-<-)"
  [ value "<" ((<) :: Comparison ())
  , value "<" ((<) :: Comparison Int)
  , value "<" ((<) :: Comparison Bool)
  , value "<" ((<) :: Comparison Char)
  , value "<" ((<) :: Comparison [Int])
  , value "<" ((<) :: Comparison [Bool])
  , value "<" ((<) :: Comparison [Char])
  , value "<" ((<) :: Comparison [[Int]])
  , value "<" ((<) :: Comparison [[Bool]])
  , value "<" ((<) :: Comparison [[Char]])
  , value "<" ((<) :: Comparison (Int,Int))
  , value "<" ((<) :: Comparison (Bool,Bool))
  , value "<" ((<) :: Comparison (Char,Char))
  , value "<" ((<) :: Comparison (Maybe Int))
  , value "<" ((<) :: Comparison (Int,Int,Int))
  ]
infix 4 -<-

-- | A function @if :: Bool -> a -> a -> a@ lifted over the 'Expr' type
--   that encodes if-then-else functionality.
--   This is properly displayed as an if-then-else.
--
-- > > if' pp zero xx
-- > (if p then 0 else x) :: Int
--
-- > > zz -*- if' pp xx yy
-- > z * (if p then x else y) :: Int
--
-- > > if' pp false true -||- if' qq true false
-- > (if p then False else True) || (if q then True else False) :: Bool
--
-- > > evl $ if' true (val 't') (val 'f') :: Char
-- > 't'
if' :: Expr -> Expr -> Expr -> Expr
if'  =  mk3 "if'"
  [ value "if" (iff :: If ())
  , value "if" (iff :: If Int)
  , value "if" (iff :: If Bool)
  , value "if" (iff :: If Char)
  , value "if" (iff :: If [Int])
  , value "if" (iff :: If [Bool])
  , value "if" (iff :: If [Char])
  , value "if" (iff :: If [[Int]])
  , value "if" (iff :: If [[Bool]])
  , value "if" (iff :: If [[Char]])
  ]
  where
  iff :: Bool -> a -> a -> a
  iff p x y  =  if p then x else y
type If a  =  Bool -> a -> a -> a

-- | A function @case :: Bool -> a -> a -> a@ lifted over the 'Expr' type
--   that encodes case-of-False-True functionality.
--   This is properly displayed as a case-of-False-True expression.
--
-- > > caseBool pp zero xx
-- > (case p of False -> 0; True -> x) :: Int
--
-- > > zz -*- caseBool pp xx yy
-- > z * (case p of False -> x; True -> y) :: Int
--
-- > > caseBool pp false true -||- caseBool qq true false
-- > (caseBool p of False -> False; True -> True) || (caseBool q of False -> True; True -> False) :: Bool
--
-- > > evl $ caseBool true (val 'f') (val 't') :: Char
-- > 't'
--
-- By convention, the 'False' case comes before 'True'
-- as @False < True@ and @data Bool = False | True@.
--
-- When evaluating, this is equivalent to if with arguments reversed.
-- Instead of using this, you are perhaps better of using if encoded as an
-- expression.  This is just here to be consistent with 'caseOrdering'.
caseBool :: Expr -> Expr -> Expr -> Expr
caseBool  =  mk3 "caseBool"
  [ value "case" (caseB :: CaseB ())
  , value "case" (caseB :: CaseB Int)
  , value "case" (caseB :: CaseB Bool)
  , value "case" (caseB :: CaseB Char)
  , value "case" (caseB :: CaseB [Int])
  , value "case" (caseB :: CaseB [Bool])
  , value "case" (caseB :: CaseB [Char])
  ]
  where
  caseB :: Bool -> a -> a -> a
  caseB p x y  =  case p of
                  False -> x
                  True -> y
type CaseB a  =  Bool -> a -> a -> a

-- | A function @case :: Ordering -> a -> a -> a -> a@ lifted over the 'Expr' type
--   that encodes case-of-LT-EQ-GT functionality.
--   This is properly displayed as a case-of-LT-EQ-GT expression.
--   (cf. 'caseBool')
--
-- > > caseOrdering (xx `compare'` yy) zero one two
-- > (case compare x y of LT -> 0; EQ -> 1; GT -> 2) :: Int
--
-- > > evl $ caseOrdering (val EQ) (val 'l') (val 'e') (val 'g') :: Char
-- > 'e'
--
-- By convention cases are given in 'LT', 'EQ' and 'GT' order
-- as @LT < EQ < GT@ and @data Ordering = LT | EQ | GT@.
caseOrdering :: Expr -> Expr -> Expr -> Expr -> Expr
caseOrdering  =  mk4 "caseOrdering"
  [ value "case" (caseO :: CaseO ())
  , value "case" (caseO :: CaseO Int)
  , value "case" (caseO :: CaseO Bool)
  , value "case" (caseO :: CaseO Char)
  , value "case" (caseO :: CaseO [Int])
  , value "case" (caseO :: CaseO [Bool])
  , value "case" (caseO :: CaseO [Char])
  ]
  where
  caseO :: Ordering -> a -> a -> a -> a
  caseO o x y z  =  case o of
                    LT -> x
                    EQ -> y
                    GT -> z
type CaseO a  =  Ordering -> a -> a -> a -> a

-- | Constructs an 'Expr'-encoded 'compare' operation between two 'Expr's.
--
-- > > xx `compare'` zero
-- > compare x 0 :: Ordering
--
-- > > compare' ae bee
-- > compare 'a' 'b' :: Ordering
compare' :: Expr -> Expr -> Expr
compare'  =  mk2 "compare'"
  [ value "compare" (compare :: Compare ())
  , value "compare" (compare :: Compare Int)
  , value "compare" (compare :: Compare Bool)
  , value "compare" (compare :: Compare Char)
  , value "compare" (compare :: Compare [Int])
  , value "compare" (compare :: Compare [Bool])
  , value "compare" (compare :: Compare [Char])
  , value "compare" (compare :: Compare [[Int]])
  , value "compare" (compare :: Compare [[Bool]])
  , value "compare" (compare :: Compare [[Char]])
  ]
type Compare a  =  a -> a -> Ordering

-- | 'Nothing' bound to the 'Maybe' 'Int' type encoded as an 'Expr'.
--
-- This is an alias to 'nothingInt'.
nothing :: Expr
nothing  =  nothingInt

-- | 'Nothing' bound to the 'Maybe' 'Int' type encoded as an 'Expr'.
nothingInt :: Expr
nothingInt   =  val (Nothing :: Maybe Int)

-- | 'Nothing' bound to the 'Maybe' 'Bool' type encoded as an 'Expr'.
nothingBool :: Expr
nothingBool  =  val (Nothing :: Maybe Bool)

-- | The 'Just' constructor of the 'Int' element type encoded as an 'Expr'.
justInt :: Expr
justInt      =  value "Just" (Just :: Int -> Maybe Int)

-- | The 'Just' constructor of the 'Bool' element type encoded as an 'Expr'.
justBool :: Expr
justBool     =  value "Just" (Just :: Bool -> Maybe Bool)

-- | The 'Just' constructor lifted over the 'Expr' type.
--
-- This works for the 'Bool', 'Int', 'Char' argument types
-- and their lists.
--
-- > > just zero
-- > Just 0 :: Maybe Int
-- > > just false
-- > Just False :: Maybe Bool
just :: Expr -> Expr
just  =  mk1 "just"
  [ justInt
  , justBool
  , value "Just" (Just :: Just Char)
  , value "Just" (Just :: Just [Int])
  , value "Just" (Just :: Just [Bool])
  , value "Just" (Just :: Just String)
  ]
type Just a  =  a -> Maybe a

-- | An infix synonym of 'pair'.
--
-- > > zero -|- xxs
-- (0,xs) :: (Int,[Int])
--
-- > > ae -|- (bee -:- unit cee)
-- > ('a',"bc") :: (Char,[Char])
(-|-) :: Expr -> Expr -> Expr
(-|-)  =  pair

-- | The pair constructor lifted over 'Expr's.
--
-- This works for some variations of 'Int', 'Bool' and 'Char' element types
-- and their lists.
--
-- > > pair zero xxs
-- (0,xs) :: (Int,[Int])
--
-- Differently from 'foldPair', when this works,-
-- it always constructs a well-typed expression.
pair :: Expr -> Expr -> Expr
pair  =  mk2 "pair"
  [ value "," ((,) :: Pair1 Int)
  , value "," ((,) :: Pair1 Bool)
  , value "," ((,) :: Pair1 Char)
  , value "," ((,) :: Pair1 [Int])
  , value "," ((,) :: Pair1 [Bool])
  , value "," ((,) :: Pair1 [Char])
  , value "," ((,) :: Pair Int [Int])
  , value "," ((,) :: Pair Bool [Bool])
  , value "," ((,) :: Pair Char [Char])
  , value "," ((,) :: Pair [Int] Int)
  , value "," ((,) :: Pair [Bool] Bool)
  , value "," ((,) :: Pair [Char] Char)
  , value "," ((,) :: Pair Int Bool)
  , value "," ((,) :: Pair Bool Int)
  , value "," ((,) :: Pair Int Char)
  , value "," ((,) :: Pair Char Int)
  , value "," ((,) :: Pair Int (Int,Int))
  , value "," ((,) :: Pair (Int,Int) Int)
  , value "," ((,) :: Pair1 (Int,Int))
  , value "," ((,) :: Pair1 [[Int]])
  , value "," ((,) :: Pair1 [[Bool]])
  , value "," ((,) :: Pair1 [[Char]])
  ]
type Pair a b  =  a -> b -> (a,b)
type Pair1 a  =  Pair a a

-- | The pair constructor (@ :: ... -> (Int,Int) @) encoded as an 'Expr'.
comma :: Expr
comma  =  value "," ((,) :: Pair Int Int)

-- | The triple/trio constructor lifted over 'Expr's.
--
-- This works for some combinations
-- of the 'Int', 'Bool' and 'Char' element types
-- and their lists.
triple :: Expr -> Expr -> Expr -> Expr
triple  =  mk3 "triple"
  [ value ",," ((,,) :: Trio1 Int)
  , value ",," ((,,) :: Trio1 Bool)
  , value ",," ((,,) :: Trio1 Char)
  , value ",," ((,,) :: Trio1 [Int])
  , value ",," ((,,) :: Trio1 [Bool])
  , value ",," ((,,) :: Trio1 [Char])
  , value ",," ((,,) :: Trio Int Int [Int])
  , value ",," ((,,) :: Trio Int [Int] Int)
  , value ",," ((,,) :: Trio [Int] Int Int)
  , value ",," ((,,) :: Trio Int [Int] [Int])
  , value ",," ((,,) :: Trio [Int] Int [Int])
  , value ",," ((,,) :: Trio [Int] [Int] Int)
  , value ",," ((,,) :: Trio Int Int Bool)
  , value ",," ((,,) :: Trio Int Bool Int)
  , value ",," ((,,) :: Trio Bool Int Int)
  , value ",," ((,,) :: Trio Int Bool Bool)
  , value ",," ((,,) :: Trio Bool Int Bool)
  , value ",," ((,,) :: Trio Bool Bool Int)
  ]
type Trio a b c  =  a -> b -> c -> (a,b,c)
type Trio1 a  =  Trio a a a

-- | The quadruple constructor lifted over 'Expr's.
--
-- This works for some combinations
-- of the 'Int', 'Bool' and 'Char' element types
-- and their lists.
quadruple :: Expr -> Expr -> Expr -> Expr -> Expr
quadruple  =  mk4 "quadruple"
  [ value ",,," ((,,,) :: Quartet1 Int)
  , value ",,," ((,,,) :: Quartet1 Bool)
  , value ",,," ((,,,) :: Quartet1 Char)
  , value ",,," ((,,,) :: Quartet1 [Int])
  , value ",,," ((,,,) :: Quartet1 [Bool])
  , value ",,," ((,,,) :: Quartet1 [Char])
  ]
type Quartet a b c d  =  a -> b -> c -> d -> (a,b,c,d)
type Quartet1 a  =  Quartet a a a a

-- | The quintuple constructor lifted over 'Expr's.
--
-- This only works for the 'Int' element type.
quintuple :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr
quintuple  =  mk5 "quintuple"
  [ value ",,,," ((,,,,) :: Quintet1 Int)
  , value ",,,," ((,,,,) :: Quintet1 Bool)
  , value ",,,," ((,,,,) :: Quintet1 Char)
  , value ",,,," ((,,,,) :: Quintet1 [Int])
  , value ",,,," ((,,,,) :: Quintet1 [Bool])
  , value ",,,," ((,,,,) :: Quintet1 [Char])
  ]
type Quintet a b c d e  =  a -> b -> c -> d -> e -> (a,b,c,d,e)
type Quintet1 a  =  Quintet a a a a a

-- | The sixtuple constructor lifted over 'Expr's.
--
-- This only works for the 'Int' element type.
sixtuple :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
sixtuple  =  mk6 "sixtuple"
  [ value ",,,,," ((,,,,,) :: Sextet1 Int)
  , value ",,,,," ((,,,,,) :: Sextet1 Bool)
  , value ",,,,," ((,,,,,) :: Sextet1 Char)
  , value ",,,,," ((,,,,,) :: Sextet1 [Int])
  , value ",,,,," ((,,,,,) :: Sextet1 [Bool])
  , value ",,,,," ((,,,,,) :: Sextet1 [Char])
  ]
type Sextet a b c d e f  =  a -> b -> c -> d -> e -> f -> (a,b,c,d,e,f)
type Sextet1 a  =  Sextet a a a a a a

-- | A typed hole of @[Bool]@ type encoded as an 'Expr'.
--
-- > > bs_
-- > _ :: [Bool]
bs_ :: Expr
bs_  =  hole [bool]

-- | 'Expr' representing a variable @p' :: `[Bool]`@.
--
-- > > pps
-- > ps :: [Bool]
pps :: Expr
pps  =  var "ps" [bool]

-- | A typed hole of '[Bool]' type
--
-- > > qqs
-- > qs :: [Bool]
qqs :: Expr
qqs  =  var "qs" [bool]

-- | 'and' lifted over the 'Expr' type.
--
-- > > and' pps
-- > and ps :: Bool
--
-- > > evl (and' $ expr [False,True]) :: Bool
-- > False
and' :: Expr -> Expr
and' e  =  andE :$ e
  where
  andE  =  value "and" (and :: [Bool] -> Bool)

-- | 'or' lifted over the 'Expr' type.
--
-- > > or' pps
-- > or ps :: Bool
--
-- > > evl (or' $ expr [False,True]) :: Bool
-- > True
or' :: Expr -> Expr
or' e  =  orE :$ e
  where
  orE  =  value "or" (or :: [Bool] -> Bool)

-- | 'sum' of 'Int' elements lifted over the 'Expr' type.
--
-- > > sum' xxs
-- > sum xs :: Int
--
-- > > evl (sum' $ expr [1,2,3::Int]) :: Int
-- > 6
sum' :: Expr -> Expr
sum' e  =  sumE :$ e
  where
  sumE  =  value "sum" (sum :: [Int] -> Int)

-- | 'product' of 'Int' elements lifted over the 'Expr' type.
--
-- > > product' xxs
-- > product xs :: Int
--
-- > > evl (product' $ expr [1,2,3::Int]) :: Int
-- > 6
product' :: Expr -> Expr
product' e  =  productE :$ e
  where
  productE  =  value "product" (product :: [Int] -> Int)

headOr :: a -> [a] -> a
headOr x []     =  x
headOr _ (x:_)  =  x

-- | The '%' constructor lifted over 'Expr's.
--
-- > > val (2 :: Integer) -%- val (3 :: Integer)
-- > 2 % 3 :: Ratio Integer
--
--   This only accepts 'Expr's bound to the 'Integer' type.
(-%-) :: Expr -> Expr -> Expr
en -%- ed  =  value "%" ((%) :: Integer -> Integer -> Rational) :$ en :$ ed

-- | Function composition encoded as an 'Expr':
--
-- > > compose
-- > (.) :: (Int -> Int) -> (Int -> Int) -> Int -> Int
compose :: Expr
compose  =  value "." ((.) :: Compose Int)

-- | Function composition '.' lifted over 'Expr'.
--
-- > > absE -.- negateE
-- > abs . negate :: Int -> Int
--
-- > > absE -.- negateE :$ one
-- > (abs . negate) 1 :: Int
--
-- This works for 'Int', 'Bool', 'Char' and their lists.
(-.-) :: Expr -> Expr -> Expr
(-.-)  =  mk2 "(-.-)"
  [ value "." ((.) :: Compose ())
  , value "." ((.) :: Compose Int)
  , value "." ((.) :: Compose Bool)
  , value "." ((.) :: Compose Char)
  , value "." ((.) :: Compose [Int])
  , value "." ((.) :: Compose [Bool])
  , value "." ((.) :: Compose [Char])
  ]
type Compose a  =  (a -> a) -> (a -> a) -> (a -> a)

-- | 'map' over the 'Int' element type encoded as an 'Expr'
--
-- > > mapE
-- > map :: (Int -> Int) -> [Int] -> [Int]
mapE :: Expr
mapE  =  value "map" (map :: Map Int)

-- | 'map' lifted over 'Expr's.
--
-- > > map' absE (unit one)
-- > map abs [1] :: [Int]
map' :: Expr -> Expr -> Expr
map'  =  mk2 "map'"
  [ value "map" (map :: Map ())
  , value "map" (map :: Map Int)
  , value "map" (map :: Map Bool)
  , value "map" (map :: Map Char)
  , value "map" (map :: Map [Int])
  , value "map" (map :: Map [Bool])
  , value "map" (map :: Map [Char])
  ]
type Map a  =  (a -> a) -> [a] -> [a]


-- | 'filter' lifted over 'Expr's.
--
-- > > filter' absE (unit one)
-- > filter odd [1] :: [Int]
filter' :: Expr -> Expr -> Expr
filter'  =  mk2 "filter'"
  [ value "filter" (filter :: Filter ())
  , value "filter" (filter :: Filter Int)
  , value "filter" (filter :: Filter Bool)
  , value "filter" (filter :: Filter Char)
  , value "filter" (filter :: Filter [Int])
  , value "filter" (filter :: Filter [Bool])
  , value "filter" (filter :: Filter [Char])
  ]
type Filter a  =  (a -> Bool) -> [a] -> [a]


-- | 'enumFrom' lifted over 'Expr's.
--
-- > > enumFrom' zero
-- > enumFrom 0 :: [Int]
--
-- Works for 'Int's, 'Bool's and 'Char's.
enumFrom' :: Expr -> Expr
enumFrom'  =  mk1 "enumFrom'"
  [ value "enumFrom" (enumFrom :: EnumFrom Int)
  , value "enumFrom" (enumFrom :: EnumFrom Bool)
  , value "enumFrom" (enumFrom :: EnumFrom Char)
  ]
type EnumFrom a  =  (a -> [a])

-- | 'enumFrom' lifted over 'Expr's named as @".."@ for pretty-printing.
--
-- > > one -.. ()
-- > [1..] :: [Int]
--
-- Works for 'Int's, 'Bool's and 'Char's.
(-..) :: Expr -> () -> Expr
e -.. _  =  mk e
  where
  mk  =  mk1 "(-..)"
      [ value ".." (enumFrom :: EnumFrom Int)
      , value ".." (enumFrom :: EnumFrom Bool)
      , value ".." (enumFrom :: EnumFrom Char)
      ]

-- | 'enumFromTo' lifted over 'Expr's
--
-- > > enumFromTo' zero four
-- > enumFromTo 0 4 :: [Int]
enumFromTo' :: Expr -> Expr -> Expr
enumFromTo'  =  mk2 "enumFromTo'"
  [ value "enumFromTo" (enumFromTo :: EnumFromTo Int)
  , value "enumFromTo" (enumFromTo :: EnumFromTo Bool)
  , value "enumFromTo" (enumFromTo :: EnumFromTo Char)
  ]
type EnumFromTo a  =  (a -> a -> [a])

-- | 'enumFromTo' lifted over 'Expr's but named as @".."@ for pretty-printing.
--
-- > > zero -..- four
-- > [0..4] :: [Int]
(-..-) :: Expr -> Expr -> Expr
(-..-)  =  mk2 "(-..-)"
  [ value ".." (enumFromTo :: EnumFromTo Int)
  , value ".." (enumFromTo :: EnumFromTo Bool)
  , value ".." (enumFromTo :: EnumFromTo Char)
  ]

-- | 'enumFromThen' lifted over 'Expr's
--
-- > > enumFromThen' zero ten
-- > enumFromThen 0 10 :: [Int]
enumFromThen' :: Expr -> Expr -> Expr
enumFromThen'  =  mk2 "enumFromThen'"
  [ value "enumFromThen" (enumFromThen :: EnumFromThen Int)
  , value "enumFromThen" (enumFromThen :: EnumFromThen Bool)
  , value "enumFromThen" (enumFromThen :: EnumFromThen Char)
  ]
type EnumFromThen a  =  (a -> a -> [a])

-- | 'enumFromThen' lifted over 'Expr's but named as @",.."@ for pretty printing.
--
-- > > (zero,ten) --.. ()
-- > [0,10..] :: [Int]
(--..) :: (Expr,Expr) -> () -> Expr
(e1,e2) --.. _  =  mk e1 e2
  where
  mk  =  mk2 "(--..)"
      [ value ",.." (enumFromThen :: EnumFromThen Int)
      , value ",.." (enumFromThen :: EnumFromThen Bool)
      , value ",.." (enumFromThen :: EnumFromThen Char)
      ]

-- | 'enumFromThenTo' lifted over 'Expr's.
--
-- > > enumFromThenTo' zero two ten
-- > enumFromThenTo 0 2 10 :: [Int]
enumFromThenTo' :: Expr -> Expr -> Expr -> Expr
enumFromThenTo'  =  mk3 "enumFromThenTo'"
  [ value "enumFromThenTo" (enumFromThenTo :: EnumFromThenTo Int)
  , value "enumFromThenTo" (enumFromThenTo :: EnumFromThenTo Bool)
  , value "enumFromThenTo" (enumFromThenTo :: EnumFromThenTo Char)
  ]
type EnumFromThenTo a  =  (a -> a -> a -> [a])

-- | 'enumFromThenTo' lifted over 'Expr's but named as @",.."@ for pretty-printing.
--
-- > > (zero,two) --..- ten
-- > [0,2..10] :: [Int]
(--..-) :: (Expr,Expr) -> Expr -> Expr
(e1, e2) --..- e3  =  mk e1 e2 e3
  where
  mk  =  mk3 "(--..-)"
      [ value ",.." (enumFromThenTo :: EnumFromThenTo Int)
      , value ",.." (enumFromThenTo :: EnumFromThenTo Bool)
      , value ",.." (enumFromThenTo :: EnumFromThenTo Char)
      ]

mk1 :: String -> [Expr] -> Expr -> Expr
mk1 nm efs ex  =  headOr (err nm efs [ex]) $ efs >$$ ex

mk2 :: String -> [Expr] -> Expr -> Expr -> Expr
mk2 nm efs ex ey  =  headOr (err nm efs [ex,ey]) $ efs >$$ ex >$$ ey

mk3 :: String -> [Expr] -> Expr -> Expr -> Expr -> Expr
mk3 nm efs ex ey ez  =  headOr (err nm efs [ex,ey,ez])
                     $  efs >$$ ex >$$ ey >$$ ez

mk4 :: String -> [Expr] -> Expr -> Expr -> Expr -> Expr -> Expr
mk4 nm efs ex ey ez ew  =  headOr (err nm efs [ex,ey,ez,ew])
                        $  efs >$$ ex >$$ ey >$$ ez >$$ ew

mk5 :: String -> [Expr] -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
mk5 nm efs ex ey ez ew ev  =  headOr (err nm efs [ex,ey,ez,ew,ev])
                           $  efs >$$ ex >$$ ey >$$ ez >$$ ew >$$ ev

mk6 :: String -> [Expr] -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
mk6 nm efs ex ey ez ew ev eu  =  headOr (err nm efs [ex,ey,ez,ew,ev,eu])
                              $  efs >$$ ex >$$ ey >$$ ez >$$ ew >$$ ev >$$ eu

err :: String -> [Expr] -> [Expr] -> Expr
err fn efs exs  =  error
                $  fn ++ ": unhandled type: "
                ++ intercalate ", " (map show exs)
                ++ "\n    accepted types are:\n"
                ++ unlines (map (("    " ++) . show) efs)
