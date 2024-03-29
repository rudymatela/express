-- |
-- Module      : Test
-- Copyright   : (c) 2019-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module defines utilities used to test "Data.Express".
--
-- It should never be exported in @ express.cabal @.
module Test
  ( module Test.LeanCheck
  , module Test.LeanCheck.Derive
  , module Test.LeanCheck.Utils
  , module Data.Express.Fixtures
  , module Data.Express.Utils.List
  , module Data.Express.Utils.Typeable
  , module Test.ListableExpr
  , module Data.Maybe
  , module Data.Either
  , module Data.Monoid
  , mainTest

  , tyBool
  , tyInt
  , tyChar
  , tyInts
  , tyIntToInt

  , allRules
  , boolRules
  , intRules
  , listRules
  )
where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Data.List (elemIndices)
import Data.Typeable (TypeRep, typeOf)

import Data.Maybe
import Data.Either
import Data.Monoid
import Test.LeanCheck
import Test.LeanCheck.Utils
import Test.LeanCheck.Derive
import Test.ListableExpr
import Data.Express.Fixtures
import Data.Express.Utils.List
import Data.Express.Utils.Typeable

reportTests :: String -> [Bool] -> IO ()
reportTests s tests =
  case elemIndices False tests of
    [] -> putStrLn $ s ++ ": tests passed"
    is -> do putStrLn (s ++ ": failed tests:" ++ show is)
             exitFailure

getMaxTestsFromArgs :: Int -> IO Int
getMaxTestsFromArgs n  =  do
  as <- getArgs
  return $ case as of
             (s:_) -> read s
             _     -> n

mainTest :: (Int -> [Bool]) -> Int -> IO ()
mainTest tests n'  =  do
  pn <- getProgName
  n <- getMaxTestsFromArgs n'
  reportTests pn (tests n)

tyBool :: TypeRep
tyBool  =  typeOf (undefined :: Bool)

tyInt :: TypeRep
tyInt  =  typeOf (undefined :: Int)

tyChar  :: TypeRep
tyChar  =  typeOf (undefined :: Char)

tyInts :: TypeRep
tyInts =  typeOf (undefined :: [Int])

tyIntToInt :: TypeRep
tyIntToInt  =  typeOf (undefined :: Int -> Int)


-- |
-- To be used when testing or benchmarking 'Triexpr'
allRules :: [(Expr,Expr)]
allRules  =  boolRules ++ intRules ++ listRules ++ boolintRules ++ funRules

boolRules :: [(Expr,Expr)]
boolRules  =
  [               id' pp  -=-  pp
  , pp -&&- pp            -=-  pp
  , pp -||- pp            -=-  pp
  , pp -&&- qq            -=-  qq -&&- pp
  , pp -||- qq            -=-  qq -||- pp
  , not' (not' pp)        -=-  pp
  , pp -&&- true          -=-  pp
  , true -&&- pp          -=-  pp
  , pp -&&- false         -=-  false
  , false -&&- pp         -=-  false
  , pp -||- true          -=-  true
  , true -||- pp          -=-  true
  , pp -||- false         -=-  pp
  , false -||- pp         -=-  pp
  , pp -&&- not' pp       -=-  false
  , pp -||- not' pp       -=-  true
  , not' pp -&&- pp       -=-  false
  , not' pp -||- pp       -=-  true
  , (pp -&&- qq) -&&- rr  -=-  pp -&&- (qq -&&- rr)
  , (pp -||- qq) -||- rr  -=-  pp -||- (qq -||- rr)
  , not' (pp -&&- qq)     -=-  not' pp -||- not' qq
  , not' (pp -||- qq)     -=-  not' pp -&&- not' qq
  , not' false            -=-  true
  , not' true             -=-  false
  , not' (not' pp -&&- not' qq) -=- (pp -||- qq)
  , not' (not' pp -||- not' qq) -=- (pp -&&- qq)
  ,   pp -&&- not' (pp -&&- qq) -=- pp -&&- not' qq
--, pp -=- pp
  ]

intRules :: [(Expr,Expr)]
intRules  =
  [               id' xx  -=-  xx
  , abs' (abs' xx)        -=-  abs' xx
  , xx -+- zero           -=-  xx
  , zero -+- xx           -=-  xx
  , xx -*- one            -=-  xx
  , one -*- xx            -=-  xx
  , xx -*- zero           -=-  zero
  , zero -*- xx           -=-  zero
  , xx -+- yy             -=-  yy -+- xx
  , xx -*- yy             -=-  yy -*- xx
  , (xx -+- yy) -+- zz    -=-  xx -+- (yy -+- zz)
  , (xx -*- yy) -*- zz    -=-  xx -*- (yy -*- zz)
  , (xx -+- xx) -*- yy    -=-  xx -*- (yy -+- yy)
  , xx -*- (yy -+- one)   -=-  xx -+- xx -*- yy
  , (xx -+- one) -*- yy   -=-  xx -+- xx -*- yy
  , xx -*- (yy -+- zz)    -=-  xx -*- yy -+- xx -*- zz
  , (xx -+- yy) -*- zz    -=-  xx -*- zz -+- yy -*- zz
  , negate' (negate' xx)  -=-  xx
  , xx -+- negate' xx     -=-  zero
  , negate' xx -+- xx     -=-  zero
  ,          abs' (negate' xx)  -=-  abs' xx
  ,                 two -*- xx  -=-  xx -+- xx
  ,                 xx -*- two  -=-  xx -+- xx
  ,               three -*- xx  -=-  xx -+- (xx -+- xx)
  ,               xx -*- three  -=-  xx -+- (xx -+- xx)
  ,                four -*- xx  -=-  xx -+- (xx -+- (xx -+- xx))
  ,                xx -*- four  -=-  xx -+- (xx -+- (xx -+- xx))
  ,           abs' (xx -*- xx)  -=-  xx -*- xx
  ,        abs' xx -*- abs' yy  -=-  abs' (xx -*- yy)
  ,        abs' xx -*- abs' xx  -=-  abs' (xx -+- xx)
  , abs' (abs' xx -+- abs' yy)  -=-  abs' xx -+- abs' yy
  ,    abs' (xx -+- xx) -*- yy  -=-  abs' xx -*- yy -+- abs' xx -*- yy
  ,     abs' xx -*- signum' xx  -=-  xx
  ,     signum' xx -*- abs' xx  -=-  xx
--, xx -=- xx
  ]

listRules :: [(Expr,Expr)]
listRules  =
  [                 id' xxs  -=-  xxs
  , head' (xx -:- xxs)       -=-  xx
  , tail' (xx -:- xxs)       -=-  xxs
  , xxs -++- nil             -=-  xxs
  , nil -++- xxs             -=-  xxs
  , unit xx -++- xxs         -=-  xx -:- xxs
  , (xx -:- xxs) -++- yys    -=-  xx -:- (xxs -++- yys)
  , (xxs -++- yys) -++- zzs  -=-  xxs -++- (yys -++- zzs)

  -- insertsort stuff
  ,        elem' xx (sort' xxs)  -=-  elem' xx xxs
  ,   elem' xx (insert' yy xxs)  -=-  elem' xx (yy -:- xxs)
  ,           sort' (sort' xxs)  -=-  sort' xxs
  ,              insert' xx nil  -=-  unit xx
  ,        sort' (xxs -++- yys)  -=-  sort' (yys -++- xxs)
  ,      sort' (insert' xx xxs)  -=-  insert' xx (sort' xxs)
  ,          sort' (xx -:- xxs)  -=-  insert' xx (sort' xxs)
  ,  sort' (xxs -++- sort' yys)  -=-  sort' (xxs -++- yys)
  ,  sort' (sort' xxs -++- yys)  -=-  sort' (xxs -++- yys)
  , insert' xx (insert' yy xxs)  -=-  insert' yy (insert' xx xxs)
  ,     insert' xx (xx -:- xxs)  -=-  xx -:- xx -:- xxs
  ,        insert' xx (unit yy)  -=-  insert' yy (unit xx)

  -- length stuff
  ,              length' (xx -:- xxs)  -=-  length' (yy -:- xxs)
  ,            length' (xxs -++- yys)  -=-  length' (yys -++- xxs)
  ,       length' (xx -:- yy -:- xxs)  -=-  length' (zz -:- xx' -:- xxs)
  ,   length' (xx -:- (xxs -++- yys))  -=-  length' (yy -:- (yys -++- xxs))
  , length' (xxs -++- (yys -++- zzs))  -=-  length' (xxs -++- (zzs -++- yys))

  ]

boolintRules :: [(Expr,Expr)]
boolintRules  =
  [ not' (odd' xx) -=- even' xx
  , not' (even' xx) -=- odd' xx
  , (xx -==- xx) -=- true
  , (xx -/=- xx) -=- false
  , (pp -==- pp) -=- true
  , (pp -/=- pp) -=- false
  ]

funRules :: [(Expr,Expr)]
funRules  =
  [ ff (gg xx)  -=-  (ffE -.- ggE) :$ xx
  , map' idE xxs  -=-  xxs
  , map' (ffE -.- ggE) xxs  -=-  map' ffE (map' ggE xxs)
  , ffE -.- idE  -=-  ffE
  , idE -.- ffE  -=-  ffE
  , (ffE -.- ggE) -.- hhE  -=-  ffE -.- (ggE -.- hhE)
  , notE -.- notE  -=-  idBool
  ]

(-=-) :: Expr -> Expr -> (Expr,Expr)
e1 -=- e2  =  (e1, e2)
infix 0 -=-
