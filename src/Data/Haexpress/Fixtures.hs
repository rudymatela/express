-- |
-- Module      : Data.Haexpress.Fixtures
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines some 'Expr' fixtures to facilitate testing and playing around on
-- the REPL (GHCI).
module Data.Haexpress.Fixtures
  (
  -- * Convenience re-export
    module Data.Haexpress

  -- * Functions and values encoded as Expr or functions of Exprs
  -- | The naming rules are:
  --
  -- * Terminal values are named in words (e.g.: 'zero', 'bee', 'cee', 'dee', 'false').
  -- * Variables have the first character duplicated (e.g.: 'xx', 'yy', 'xxs');
  -- * Encoded functions are followed by @E@ (e.g.: 'idE', 'plus');
  -- * Lifted functions are primed (e.g.: 'id'', 'negate'');
  -- * Lifted operators are surrounded by dashes (e.g.: '-+-', '-*-').

  -- ** Booleans
  , b_, pp, qq
  , false
  , true
  , notE
  , orE
  , andE
  , impliesE
  , not'
  , (-||-)
  , (-&&-)
  , (-==>-)
  , (-==-)
  , (-/=-)
  , (-<=-)
  , (-<-)

  -- ** Integers
  , i_, xx, yy, zz, xx'
  , ii, jj, kk, ii'
  , zero, one, two, three, minusOne, minusTwo
  , idE, negateE, absE
  , id', const', negate', abs'
  , plus, times
  , (-+-), (-*-)
  , ff, ffE
  , gg, ggE
  , (-?-), iiE
  , (-$-)
  , odd'
  , even'

  -- ** Chars
  , c_
  , cc, dd, ccs
  , ae, bee, cee, dee
  , space, lineBreak
  , ord'
  , ordE

  -- ** Lists
  , is_
  , xxs
  , yys
  , nil
  , emptyString
  , cons
  , (-:-)
  , unit
  , (-++-)
  , head'
  , tail'
  , elem'
  , sort'
  , insert'

  , comma
  , (-|-)
  , triple
  , quadruple
  , quintuple
  , sixtuple
  )
where

import Data.Haexpress
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

rr :: Expr -- ar, I'm a pirate
rr  =  var "r" bool


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
e1 -==>- e2  =  impliesE :$ e1 :$ e2

impliesE :: Expr
impliesE  =  value "==>" (==>)
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

ii :: Expr
ii  =  var "i" int

jj :: Expr
jj  =  var "j" int

kk :: Expr
kk  =  var "k" int

ii' :: Expr
ii'  =  var "i'" int

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

-- | A variable function @?@ of type @Int -> Int -> Int@ encoded as an 'Expr'.
--
-- > > iiE
-- > (?) :: Int -> Int -> Int
--
-- > > iiE :$ xx
-- > (x ?) :: Int -> Int
--
-- > > iiE :$ xx :$ yy
-- > x ? y :: Int
iiE :: Expr
iiE  =  var "?" (?)
  where
  (?) :: Int -> Int -> Int
  x ? y  =  undefined

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
  [ iiE
  , var "?" (undefined :: Bool -> Bool -> Bool)
  , var "?" (undefined :: Char -> Char -> Char)
  , var "?" (undefined :: [Int] -> [Int] -> [Int])
  , var "?" (undefined :: String -> String -> String)
  ]
  where
  err  =  error $ "(-?-): cannot apply `(?) :: * -> * -> *` to `"
               ++ show ex ++ "' and `" ++ show ey ++ "'.  Unhandled types?"

-- | A variable @g@ of 'Int -> Int' type encoded as an 'Expr'.
--
-- > > ggE
-- > g :: Int -> Int
ggE :: Expr
ggE = var "g" (undefined :: Int -> Int)

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
  [ idE -- :: Int -> Int
  , value "id" (id :: Bool -> Bool)
  , value "id" (id :: Char -> Char)
  , value "id" (id :: [Int] -> [Int])
  , value "id" (id :: [Bool] -> [Bool])
  , value "id" (id :: String -> String)
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
idE  =  value "id" (id :: Int -> Int)

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

-- | An empty list of type @[Int]@ encoded as an 'Expr'.
--
-- > > nil
-- > [] :: [Int]
nil :: Expr
nil  =  val ([] :: [Int])

-- | An empty 'String' encoded as an 'Expr'.
--
-- > > emptyString
-- > "" :: String
emptyString :: Expr
emptyString  =  val ""

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
cons = value ":" ((:) :: Int -> [Int] -> [Int])

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
       | typ e == typ b_  =  val ([] :: [Bool])

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
  [ cons
  , value ":" ((:) :: Char -> String -> String)
  , value ":" ((:) :: Bool -> [Bool] -> [Bool])
  ]
  where
  err  =  error $ "(-:-): unhandled type " ++ show (typ e1)
infixr 5 -:-

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

(-|-) :: Expr -> Expr -> Expr
e1 -|- e2 = comma :$ e1 :$ e2

comma :: Expr
comma = value "," ((,) :: Int -> Int -> (Int,Int))

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

headOr :: a -> [a] -> a
headOr x []     =  x
headOr _ (x:_)  =  x
