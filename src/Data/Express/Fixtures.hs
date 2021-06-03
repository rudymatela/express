-- |
-- Module      : Data.Express.Fixtures
-- Copyright   : (c) 2019-2021 Rudy Matela
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
-- encoded polimorphic values are monomorphized
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

  -- ** Integers
  , i_, xx, yy, zz, xx'
  , ii, jj, kk, ii'
  , ll, mm, nn
  , zero, one, two, three, four, minusOne, minusTwo
  , idE, negateE, absE, signumE
  , idInt
  , idBool
  , idChar
  , idInts
  , idBools
  , idString
  , id', const', negate', abs', signum'
  , plus, times
  , (-+-), (-*-)
  , ff, ffE
  , gg, ggE
  , hh, hhE
  , (-?-)
  , (-$-)
  , odd'
  , even'

  -- ** Chars
  , c_
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

  -- ** Higher order
  , compose
  , mapE
  , (-.-)
  , map'

  -- ** Enum
  , enumFrom',   (-..)
  , enumFromTo', (-..-)
  , enumFromThen', (-...)
  , enumFromThenTo', (-...-)
  )
where

import Data.Express
import Data.Maybe
import Data.Typeable (Typeable, typeOf)
import Data.Char
import Data.List

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

(-==>-) :: Expr -> Expr -> Expr
e1 -==>- e2  =  implies :$ e1 :$ e2
infixr 0 -==>-

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

-- | A variable function @f@ of 'Int -> Int' type lifted over the 'Expr' type.
--
-- > > ff xx
-- > f x :: Int
--
-- > > ff one
-- > f 1 :: Int
ff :: Expr -> Expr
ff = (ffE :$)

-- | A variable @f@ of 'Int -> Int' type encoded as an 'Expr'.
--
-- > > ffE
-- > f :: Int -> Int
ffE :: Expr
ffE = var "f" (undefined :: Int -> Int)

-- | A variable function @g@ of 'Int -> Int' type lifted over the 'Expr' type.
--
-- > > gg yy
-- > g y :: Int
--
-- > > gg minusTwo
-- > gg (-2) :: Int
gg :: Expr -> Expr
gg = (ggE :$)

-- | A variable @g@ of 'Int -> Int' type encoded as an 'Expr'.
--
-- > > ggE
-- > g :: Int -> Int
ggE :: Expr
ggE = var "g" (undefined :: Int -> Int)

-- | A variable function @h@ of 'Int -> Int' type lifted over the 'Expr' type.
--
-- > > hh zz
-- > h z :: Int
hh :: Expr -> Expr
hh = (hhE :$)

-- | A variable @h@ of 'Int -> Int' type encoded as an 'Expr'.
--
-- > > hhE
-- > h :: Int -> Int
hhE :: Expr
hhE = var "h" (undefined :: Int -> Int)

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
-- > *** Exception: (-?-): cannot apply `(?) :: * -> * -> *` to `x :: Int' and `q :: Bool'.  Unhandled types?
(-?-) :: Expr -> Expr -> Expr
ex -?- ey  =  fromMaybe err $ ($$ ey) $ headOr err $ mapMaybe ($$ ex)
  [ var "?" (undefined :: Int -> Int -> Int)
  , var "?" (undefined :: Bool -> Bool -> Bool)
  , var "?" (undefined :: Char -> Char -> Char)
  , var "?" (undefined :: [Int] -> [Int] -> [Int])
  , var "?" (undefined :: String -> String -> String)
  ]
  where
  err  =  error $ "(-?-): cannot apply `(?) :: * -> * -> *` to `"
               ++ show ex ++ "' and `" ++ show ey ++ "'.  Unhandled types?"

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
e1 -+- e2 = plus :$ e1 :$ e2
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
plus = value "+" ((+) :: Int -> Int -> Int)

-- | The operator '*' for the 'Int' type lifted over the 'Expr' type.  (See also 'times'.)
--
-- > > three -*- three
-- > 9 :: Int
--
-- > > one -*- two -*- three
-- > (1 * 2) * 3 :: Int
--
-- > > two -*- xx
-- > 2 * x :: Int
(-*-) :: Expr -> Expr -> Expr
e1 -*- e2 = times :$ e1 :$ e2
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

minus :: Expr
minus  =  value "-" ((-) :: Int -> Int -> Int)

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
id' e  =  headOr err $ mapMaybe ($$ e)
  [ idInt
  , idBool
  , idChar
  , idInts
  , idBools
  , idString
  ]
  where
  err  =  error $ "id': unhandled type " ++ show (typ e)

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
type Id a = a -> a

const' :: Expr -> Expr -> Expr
const' e1 e2  =  (:$ e2) . headOr err $ mapMaybe ($$ e1)
  [ value "const" (const :: Int -> Int -> Int)
  , value "const" (const :: Bool -> Bool -> Bool)
  , value "const" (const :: Char -> Char -> Char)
  , value "const" (const :: [Int] -> [Int] -> [Int])
  , value "const" (const :: [Bool] -> [Bool] -> [Bool])
  , value "const" (const :: String -> String -> String)
  ]
  where
  err  =  error $ "const': unhandled type " ++ show (typ e1)

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

odd' :: Expr -> Expr
odd' = (oddE :$) where oddE = value "odd" (odd :: Int -> Bool)

even' :: Expr -> Expr
even' = (evenE :$) where evenE = value "even" (even :: Int -> Bool)

-- | A hole of 'Char' type encoded as an 'Expr'.
--
-- > > c_
-- > _ :: Char
c_ :: Expr
c_  =  hole char

cc :: Expr
cc  =  var "c" char

dd :: Expr
dd  =  var "d" char

ccs :: Expr
ccs  =  var "cs" [char]

-- The English name for letter 'a' is not really 'ae', but simply 'a'.
ae :: Expr
ae  =  val 'a'

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

space :: Expr
space = val ' '

lineBreak :: Expr
lineBreak = val '\n'

ord' :: Expr -> Expr
ord' = (ordE :$)

ordE :: Expr
ordE = value "ord" ord

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
type Cons a = a -> [a] -> [a]

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
  nil' | typ e == typ i_  =  nil
       | typ e == typ c_  =  emptyString
       | typ e == typ b_  =  nilBool

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
e1 -:- e2  =  (:$ e2) . headOr err $ mapMaybe ($$ e1)
  [ consInt
  , consBool
  , consChar
  , value ":" ((:) :: Cons (Maybe Int))
  ]
  where
  err  =  error $ "(-:-): unhandled type " ++ show (typ e1)
infixr 5 -:-

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
e1 -++- e2 = (:$ e2) . headOr err $ mapMaybe ($$ e1)
  [ value "++" ((++) :: [Int] -> [Int] -> [Int])
  , value "++" ((++) :: String -> String -> String)
  , value "++" ((++) :: [Bool] -> [Bool] -> [Bool])
  ]
  where
  err  =  error $ "(-++-): unhandled type " ++ show (typ e1)
infixr 5 -++-

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
head' exs = headOr err $ mapMaybe ($$ exs)
  [ value "head" (head :: [Int] -> Int)
  , value "head" (head :: [Char] -> Char)
  , value "head" (head :: [Bool] -> Bool)
  ]
  where
  err  =  error $ "head': cannot apply `head :: [a] -> a` to `" ++ show exs ++ "'."

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
tail' exs = headOr err $ mapMaybe ($$ exs)
  [ value "tail" (tail :: [Int] -> [Int])
  , value "tail" (tail :: [Char] -> [Char])
  , value "tail" (tail :: [Bool] -> [Bool])
  ]
  where
  err  =  error $ "tail': unhandled type " ++ show (typ exs)

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
null' exs = headOr err $ mapMaybe ($$ exs)
  [ value "null" (null :: [Int] -> Bool)
  , value "null" (null :: [Char] -> Bool)
  , value "null" (null :: [Bool] -> Bool)
  ]
  where
  err  =  error $ "null': unhandled type " ++ show (typ exs)

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
length' exs = headOr err $ mapMaybe ($$ exs)
  [ value "length" (length :: [Int] -> Int)
  , value "length" (length :: [Char] -> Int)
  , value "length" (length :: [Bool] -> Int)
  ]
  where
  err  =  error $ "length': cannot apply `length :: [a] -> a` to `" ++ show exs ++ "'."

sort' :: Expr -> Expr
sort' exs = headOr err $ mapMaybe ($$ exs)
  [ value "sort" (sort :: [Int] -> [Int])
  , value "sort" (sort :: [Char] -> [Char])
  , value "sort" (sort :: [Bool] -> [Bool])
  ]
  where
  err  =  error $ "sort': unhandled type " ++ show (typ exs)

insert' :: Expr -> Expr -> Expr
insert' ex exs  =  (:$ exs) . headOr err $ mapMaybe ($$ ex)
  [ value "insert" (insert :: Int -> [Int] -> [Int])
  , value "insert" (insert :: Bool -> [Bool] -> [Bool])
  , value "insert" (insert :: Char -> String -> String)
  ]
  where
  err  =  error $ "insert': unhandled type " ++ show (typ ex)

elem' :: Expr -> Expr -> Expr
elem' ex exs  =  (:$ exs) . headOr err $ mapMaybe ($$ ex)
  [ value "elem" (elem :: Int -> [Int] -> Bool)
  , value "elem" (elem :: Bool -> [Bool] -> Bool)
  , value "elem" (elem :: Char -> String -> Bool)
  ]
  where
  err  =  error $ "elem': unhandled type " ++ show (typ ex)

(-$-) :: Expr -> Expr -> Expr
ef -$- ex = (:$ ex) . headOr err $ mapMaybe ($$ ef)
  [ value "$" (($) :: Apply Int)
  , value "$" (($) :: Apply Bool)
  , value "$" (($) :: Apply Char)
  , value "$" (($) :: Apply [Int])
  , value "$" (($) :: Apply [Bool])
  , value "$" (($) :: Apply [Char])
  ]
  where
  err  =  error $ "(-$-): unhandled type " ++ show (typ ef)
infixl 6 -$-
type Apply a = (a -> a) -> a -> a

(-==-) :: Expr -> Expr -> Expr
ex -==- ey  =  (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value "==" ((==) :: Comparison ())
  , value "==" ((==) :: Comparison Int)
  , value "==" ((==) :: Comparison Bool)
  , value "==" ((==) :: Comparison Char)
  , value "==" ((==) :: Comparison [Int])
  , value "==" ((==) :: Comparison [Bool])
  , value "==" ((==) :: Comparison [Char])
  ]
  where
  err  =  error $ "(-==-): unhandled type " ++ show (typ ex)
infix 4 -==-
type Comparison a = a -> a -> Bool

(-/=-) :: Expr -> Expr -> Expr
ex -/=- ey  =  (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value "/=" ((/=) :: Comparison ())
  , value "/=" ((/=) :: Comparison Int)
  , value "/=" ((/=) :: Comparison Bool)
  , value "/=" ((/=) :: Comparison Char)
  , value "/=" ((/=) :: Comparison [Int])
  , value "/=" ((/=) :: Comparison [Bool])
  , value "/=" ((/=) :: Comparison [Char])
  ]
  where
  err  =  error $ "(-/=-): unhandled type " ++ show (typ ex)
infix 4 -/=-

(-<=-) :: Expr -> Expr -> Expr
ex -<=- ey  =  (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value "<=" ((<=) :: Comparison ())
  , value "<=" ((<=) :: Comparison Int)
  , value "<=" ((<=) :: Comparison Bool)
  , value "<=" ((<=) :: Comparison Char)
  , value "<=" ((<=) :: Comparison [Int])
  , value "<=" ((<=) :: Comparison [Bool])
  , value "<=" ((<=) :: Comparison [Char])
  ]
  where
  err  =  error $ "(-<=-): unhandled type " ++ show (typ ex)
infix 4 -<=-

(-<-) :: Expr -> Expr -> Expr
ex -<- ey  =  (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value "<" ((<) :: Comparison ())
  , value "<" ((<) :: Comparison Int)
  , value "<" ((<) :: Comparison Bool)
  , value "<" ((<) :: Comparison Char)
  , value "<" ((<) :: Comparison [Int])
  , value "<" ((<) :: Comparison [Bool])
  , value "<" ((<) :: Comparison [Char])
  ]
  where
  err  =  error $ "(-<-): unhandled type " ++ show (typ ex)
infix 4 -<-

-- | A virtual function @if :: Bool -> a -> a -> a@ lifted over the 'Expr' type.
--   This is displayed as an if-then-else.
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
if' ep ex ey  =  (:$ ey) . headOr err . mapMaybe ($$ ex) $ map (:$ ep)
  [ value "if" (iff :: If ())
  , value "if" (iff :: If Int)
  , value "if" (iff :: If Bool)
  , value "if" (iff :: If Char)
  , value "if" (iff :: If [Int])
  , value "if" (iff :: If [Bool])
  , value "if" (iff :: If [Char])
  ]
  where
  err  =  error $ "if': unhandled type " ++ show (typ ex)
  iff :: Bool -> a -> a -> a
  iff p x y  =  if p then x else y
type If a = Bool -> a -> a -> a

compare' :: Expr -> Expr -> Expr
compare' ex ey  =  (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value "compare" (compare :: Compare ())
  , value "compare" (compare :: Compare Int)
  , value "compare" (compare :: Compare Bool)
  , value "compare" (compare :: Compare Char)
  , value "compare" (compare :: Compare [Int])
  , value "compare" (compare :: Compare [Bool])
  , value "compare" (compare :: Compare [Char])
  ]
  where
  err  =  error $ "(-<-): unhandled type " ++ show (typ ex)
type Compare a = a -> a -> Ordering

nothing :: Expr
nothing  =  nothingInt

nothingInt, nothingBool :: Expr
nothingInt   =  val (Nothing :: Maybe Int)
nothingBool  =  val (Nothing :: Maybe Bool)

justInt, justBool :: Expr
justInt      =  value "Just" (Just :: Just Int)
justBool     =  value "Just" (Just :: Just Bool)
type Just a  =  a -> Maybe a

just :: Expr -> Expr
just ex  =  headOr err $ mapMaybe ($$ ex)
  [ justInt
  , justBool
  ]
  where
  err  =  error $ "just: unhandled type " ++ show (typ ex)

(-|-) :: Expr -> Expr -> Expr
(-|-) = pair

pair :: Expr -> Expr -> Expr
pair x y  =  comma :$ x :$ y
  where
  comma  =  case (show $ typ x, show $ typ y) of
            ("Int", "Int")  -> value "," ((,) :: Pair Int Int)
            ("Int", "Bool") -> value "," ((,) :: Pair Int Bool)
            ("Bool","Int")  -> value "," ((,) :: Pair Bool Int)
            ("Bool","Bool") -> value "," ((,) :: Pair Bool Bool)
            (t,t')          -> error $ "(-:-): unhandled types " ++ t ++ " " ++ t'
type Pair a b = a -> b -> (a,b)

comma :: Expr
comma = value "," ((,) :: Pair Int Int)

triple :: Expr -> Expr -> Expr -> Expr
triple e1 e2 e3 = ccE :$ e1 :$ e2 :$ e3
  where
  ccE = value ",," ((,,) :: Int -> Int -> Int -> (Int,Int,Int))

quadruple :: Expr -> Expr -> Expr -> Expr -> Expr
quadruple e1 e2 e3 e4 = cccE :$ e1 :$ e2 :$ e3 :$ e4
  where
  cccE = value ",,," ((,,,) :: Int -> Int -> Int -> Int -> (Int,Int,Int,Int))

quintuple :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr
quintuple e1 e2 e3 e4 e5 = ccccE :$ e1 :$ e2 :$ e3 :$ e4 :$ e5
  where
  ccccE = value ",,,," ((,,,,) :: Int -> Int -> Int -> Int -> Int -> (Int,Int,Int,Int,Int))

sixtuple :: Expr -> Expr -> Expr -> Expr -> Expr -> Expr -> Expr
sixtuple e1 e2 e3 e4 e5 e6 = cccccE :$ e1 :$ e2 :$ e3 :$ e4 :$ e5 :$ e6
  where
  cccccE = value ",,,,," ((,,,,,) :: Int -> Int -> Int -> Int -> Int -> Int -> (Int,Int,Int,Int,Int,Int))

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

compose :: Expr
compose  =  value "." ((.) :: Compose Int)

(-.-) :: Expr -> Expr -> Expr
ex -.- ey  =  (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value "." ((.) :: Compose ())
  , value "." ((.) :: Compose Int)
  , value "." ((.) :: Compose Bool)
  , value "." ((.) :: Compose Char)
  , value "." ((.) :: Compose [Int])
  , value "." ((.) :: Compose [Bool])
  , value "." ((.) :: Compose [Char])
  ]
  where
  err  =  error $ "(-.-): unhandled type " ++ show (typ ex)
type Compose a = (a -> a) -> (a -> a) -> (a -> a)

mapE :: Expr
mapE  =  value "map" (map :: Map Int)

map' :: Expr -> Expr -> Expr
map' ef exs  =  (:$ exs) . headOr err $ mapMaybe ($$ ef)
  [ value "map" (map :: Map ())
  , value "map" (map :: Map Int)
  , value "map" (map :: Map Bool)
  , value "map" (map :: Map Char)
  , value "map" (map :: Map [Int])
  , value "map" (map :: Map [Bool])
  , value "map" (map :: Map [Char])
  ]
  where
  err  =  error $ "map': unhandled type " ++ show (typ ef)
type Map a = (a -> a) -> [a] -> [a]

enumFrom' :: Expr -> Expr
enumFrom' ex  =  headOr err $ mapMaybe ($$ ex)
  [ value "enumFrom" (enumFrom :: EnumFrom Int)
  , value "enumFrom" (enumFrom :: EnumFrom Bool)
  , value "enumFrom" (enumFrom :: EnumFrom Char)
  ]
  where
  err  =  error $ "enumFrom': unhandled type " ++ show (typ ex)
type EnumFrom a  =  (a -> [a])

(-..) :: Expr -> Expr
(-..) ex  =  headOr err $ mapMaybe ($$ ex)
  [ value ".." (enumFrom :: EnumFrom Int)
  , value ".." (enumFrom :: EnumFrom Bool)
  , value ".." (enumFrom :: EnumFrom Char)
  ]
  where
  err  =  error $ "(-..): unhandled type " ++ show (typ ex)

enumFromTo' :: Expr -> Expr -> Expr
enumFromTo' ex ey  =  (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value "enumFromTo" (enumFromTo :: EnumFromTo Int)
  , value "enumFromTo" (enumFromTo :: EnumFromTo Bool)
  , value "enumFromTo" (enumFromTo :: EnumFromTo Char)
  ]
  where
  err  =  error $ "enumFromTo': unhandled type " ++ show (typ ex)
type EnumFromTo a  =  (a -> a -> [a])

(-..-) :: Expr -> Expr -> Expr
ex -..- ey  =  (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value ".." (enumFromTo :: EnumFromTo Int)
  , value ".." (enumFromTo :: EnumFromTo Bool)
  , value ".." (enumFromTo :: EnumFromTo Char)
  ]
  where
  err  =  error $ "-..-: unhandled type " ++ show (typ ex)

enumFromThen' :: Expr -> Expr -> Expr
enumFromThen' ex ey  =  (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value "enumFromThen" (enumFromThen :: EnumFromThen Int)
  , value "enumFromThen" (enumFromThen :: EnumFromThen Bool)
  , value "enumFromThen" (enumFromThen :: EnumFromThen Char)
  ]
  where
  err  =  error $ "enumFromThen': unhandled type " ++ show (typ ex)
type EnumFromThen a  =  (a -> a -> [a])

(-...) :: Expr -> Expr -> Expr
ex -... ey  =  (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value ",.." (enumFromThen :: EnumFromThen Int)
  , value ",.." (enumFromThen :: EnumFromThen Bool)
  , value ",.." (enumFromThen :: EnumFromThen Char)
  ]
  where
  err  =  error $ "-..-: unhandled type " ++ show (typ ex)

enumFromThenTo' :: Expr -> Expr -> Expr -> Expr
enumFromThenTo' ex ey ez  =  (:$ ez) . (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value "enumFromThenTo" (enumFromThenTo :: EnumFromThenTo Int)
  , value "enumFromThenTo" (enumFromThenTo :: EnumFromThenTo Bool)
  , value "enumFromThenTo" (enumFromThenTo :: EnumFromThenTo Char)
  ]
  where
  err  =  error $ "enumFromThenTo': unhandled type " ++ show (typ ex)
type EnumFromThenTo a  =  (a -> a -> a -> [a])

(-...-) :: Expr -> Expr -> Expr
ex -...- ey  =  (:$ ey) . headOr err $ mapMaybe ($$ ex)
  [ value ",.." (enumFromThenTo :: EnumFromThenTo Int)
  , value ",.." (enumFromThenTo :: EnumFromThenTo Bool)
  , value ",.." (enumFromThenTo :: EnumFromThenTo Char)
  ]
  where
  err  =  error $ "-..-: unhandled type " ++ show (typ ex)
