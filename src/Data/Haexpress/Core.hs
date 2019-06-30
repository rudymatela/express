-- |
-- Module      : Data.Haexpress.Core
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Expr' type and utilities involving it.
--
-- The complexity of most functions are given in big O notation
-- where /n/ is the size of the expression being manipulated or produced.
-- There may still be a /m/ cost associated with the values being stored in 'Expr's.
{-# LANGUAGE DeriveDataTypeable #-} -- for GHC < 7.10
module Data.Haexpress.Core
  (
  -- * The Expr datatype
    Expr (..)

  -- * Smart constructors
  , value
  , val
  , ($$)
  , var

  -- * Evaluating Exprs
  , evaluate
  , eval
  , evl
  , typ
  , etyp
  , mtyp
  , toDynamic

  -- * Boolean properties
  , isIllTyped
  , isWellTyped
  , hasVar
  , isGround
  , isVar
  , isConst

  -- * Comparison
  , compareComplexity

  -- * Properties
  , arity
  , size
  , depth

  -- * Listing subexpressions
  , subexprs
  , nubSubexprs
  , values
  , vars
  , consts
  , nubValues
  , nubVars
  , nubConsts

  -- * Other utilities
  , unfoldApp
  , showExpr
  , showOpExpr -- TODO: eventually remove this
  )
where

-- TODO: talk about convention of preceding variables with "_"
-- TODO: more exports
-- TODO: rename lexicompare family?
-- TODO: isList
-- TODO: unfoldList

import Data.Dynamic
import Data.Function (on)
import Data.List (intercalate, sort)
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.Either (either)
import Data.Monoid ((<>))
import Data.Typeable (TypeRep, typeOf, funResultTy, splitTyConApp, TyCon, typeRepTyCon)

import Data.Haexpress.Utils.List
import Data.Haexpress.Utils.String
import Data.Haexpress.Utils.Typeable

-- | A functional-application expression representation
data Expr  =  Value String Dynamic -- ^ a 'value' enconded as 'String' and 'Dynamic'
           |  Expr :$ Expr         -- ^ function application between expressions
  deriving Typeable -- for GHC < 7.10

-- | /O(1)./
-- It takes a string representation of a value and a value, returning an
-- 'Expr' with that terminal value.  Examples:
--
-- > > value "0" (0 :: Integer)
-- > 0 :: Integer
--
-- > > value "'a'" 'a'
-- > 'a' :: Char
--
-- > > value "True" True
-- > True :: Bool
--
-- > > value "id" (id :: Int -> Int)
-- > id :: Int -> Int
--
-- > > value "(+)" ((+) :: Int -> Int -> Int)
-- > (+) :: Int -> Int -> Int
--
-- > > value "sort" (sort :: [Bool] -> [Bool])
-- > sort :: [Bool] -> [Bool]
value :: Typeable a => String -> a -> Expr
value s x = Value s (toDyn x)

-- | /O(1)./
-- A shorthand for 'value' for values that are 'Show' instances.
-- Examples:
--
-- > > val (0 :: Int)
-- > 0 :: Int
--
-- > > val 'a'
-- > 'a' :: Char
--
-- > > val True
-- > True :: Bool
--
-- Example equivalences to 'value':
--
-- > val 0     =  value "0" 0
-- > val 'a'   =  value "'a'" 'a'
-- > val True  =  value "True" True
val :: (Typeable a, Show a) => a -> Expr
val x = value (show x) x

-- | /O(n)/.
-- Creates an 'Expr' representing a function application.
-- 'Just' an 'Expr' application if the types match, 'Nothing' otherwise.
--
-- > > value "id" (id :: () -> ()) $$ val ()
-- > Just (id () :: ())
--
-- > > value "abs" (abs :: Int -> Int) $$ val (1337 :: Int)
-- > Just (abs 1337 :: Int)
--
-- > > value "abs" (abs :: Int -> Int) $$ val 'a'
-- > Nothing
--
-- > > value "abs" (abs :: Int -> Int) $$ val ()
-- > Nothing
($$) :: Expr -> Expr -> Maybe Expr
e1 $$ e2 | isIllTyped e  =  Nothing
         | otherwise     =  Just e
  where
  e = e1 :$ e2

-- | /O(1)/.
-- Creates an 'Expr' representing a variable with the given name and argument
-- type.
--
-- > > var "x" (undefined :: Int)
-- > x :: Int
--
-- > > var "u" (undefined :: ())
-- > u :: ()
--
-- > > var "xs" (undefined :: [Int])
-- > xs :: [Int]
var :: Typeable a => String -> a -> Expr
var s a = value ('_':s) (undefined `asTypeOf` a)

-- | /O(n)/.
-- Computes the type of an expression.  This raises errors, but this should
-- not happen if expressions are smart-constructed.
--
-- > > let one = val (1 :: Int)
-- > > let bee = val 'b'
-- > > let absE = value "abs" (abs :: Int -> Int)
--
-- > > typ one
-- > Int
--
-- > > typ bee
-- > Char
--
-- > > typ absE
-- > Int -> Int
--
-- > > typ (absE :$ one)
-- > Int
--
-- > > typ (absE :$ bee)
-- > *** Exception: type mismatch, cannot apply `Int -> Int' to `Char'
--
-- > > typ ((absE :$ bee) :$ one)
-- > *** Exception: type mismatch, cannot apply `Int -> Int' to `Char'
typ :: Expr -> TypeRep
typ  =  either err id . etyp
  where
  err (t1, t2)  =  error $ "type mismatch, cannot apply `"
                ++ show t1 ++ "' to `" ++ show t2 ++ "'"

-- | /O(n)/
-- Computes the type of an expression returning either the type of the given
-- expression when possible or when there is a type error, the pair of types
-- which produced the error.
--
-- > > let one = val (1 :: Int)
-- > > let bee = val 'b'
-- > > let absE = value "abs" (abs :: Int -> Int)
--
-- > > etyp one
-- > Right Int
--
-- > > etyp bee
-- > Right Char
--
-- > > etyp absE
-- > Right (Int -> Int)
--
-- > > etyp (absE :$ one)
-- > Right Int
--
-- > > etyp (absE :$ bee)
-- > Left (Int -> Int, Char)
--
-- > > etyp ((absE :$ bee) :$ one)
-- > Left (Int -> Int, Char)
etyp :: Expr -> Either (TypeRep, TypeRep) TypeRep
etyp (Value _ d) = Right $ dynTypeRep d
etyp (e1 :$ e2) = case (etyp e1, etyp e2) of
  (Right t1, Right t2) -> case t1 `funResultTy` t2 of
                          Nothing -> Left (t1,t2)
                          Just t  -> Right t
  (Left e, _) -> Left e
  (_, Left e) -> Left e

-- | /O(n)/
-- Returns 'Just' the type of an expression
-- or 'Nothing' when there is an error.
--
-- > > let one = val (1 :: Int)
-- > > let bee = val 'b'
-- > > let absE = value "abs" (abs :: Int -> Int)
--
-- > > mtyp one
-- > Just Int
--
-- > > mtyp (absE :$ bee)
-- > Nothing
mtyp :: Expr -> Maybe TypeRep
mtyp  =  either (const Nothing) Just . etyp

-- | /O(n)/
-- Returns whether the given 'Expr' is ill typed.
--
-- > > let one = val (1 :: Int)
-- > > let bee = val 'b'
-- > > let absE = value "abs" (abs :: Int -> Int)
--
-- > > mtyp (absE :$ one)
-- > Just Int
--
-- > > mtyp (absE :$ bee)
-- > Nothing
isIllTyped :: Expr -> Bool
isIllTyped  =  isNothing . mtyp

isWellTyped :: Expr -> Bool
isWellTyped  =  isJust . mtyp

-- |  /O(n)/.
-- 'Just' the value of an expression when possible (correct type),
-- 'Nothing' otherwise.
-- This does not catch errors from 'undefined' 'Dynamic' 'value's.
--
-- > > let one = val (1 :: Int)
-- > > let bee = val 'b'
-- > > let negateE = value "negate" (negate :: Int -> Int)
--
-- > > evaluate one :: Maybe Int
-- > Just 1
--
-- > > evaluate one :: Maybe Char
-- > Nothing
--
-- > > evaluate bee :: Maybe Int
-- > Nothing
--
-- > > evaluate bee :: Maybe Char
-- > Just 'b'
--
-- > > evaluate $ negateE :$ one :: Maybe Int
-- > Just (-1)
--
-- > > evaluate $ negateE :$ bee :: Maybe Int
-- > Nothing
evaluate :: Typeable a => Expr -> Maybe a
evaluate e = toDynamic e >>= fromDynamic

-- | /O(n)/. 
-- Evaluates an expression when possible (correct type).
-- Returns a default value otherwise.
--
-- > > let two = val (2 :: Int)
-- > > let three = val (3 :: Int)
-- > > let e1 -+- e2 = value "+" ((+) :: Int->Int->Int) :$ e1 :$ e2
--
-- > > eval 0 $ two -+- three :: Int
-- > 5
--
-- > > eval 'z' $ two -+- three :: Char
-- > 'z'
--
-- > > eval 0 $ two -+- val '3' :: Int
-- > 0
eval :: Typeable a => a -> Expr -> a
eval x e = fromMaybe x (evaluate e)

-- | /O(n)/.
-- Evaluates an expression when possible (correct type).
-- Raises an error otherwise.
--
-- > > evl $ two -+- three :: Int
-- > 5
--
-- > > evl $ two -+- three :: Bool
-- > *** Exception: evl: cannot evaluate Expr `2 + 3 :: Int' at the Bool type
--
-- This may raise errors, please consider using 'eval' or 'evaluate'.
evl :: Typeable a => Expr -> a
evl e = r
  where
  r = eval err e
  err = error $ "evl: cannot evaluate Expr `" ++ show e ++ "' at the " ++ show (typeOf r) ++ " type"

-- | /O(n)/.
-- Evaluates an expression to a terminal 'Dynamic' value when possible.
-- Returns 'Nothing' otherwise.
--
-- > > toDynamic $ val (123 :: Int) :: Maybe Dynamic
-- > Just <<Int>>
--
-- > > toDynamic $ value "abs" (abs :: Int -> Int) :$ val (-1 :: Int)
-- > Just <<Int>>
--
-- > > toDynamic $ value "abs" (abs :: Int -> Int) :$ val 'a'
-- > Nothing
toDynamic :: Expr -> Maybe Dynamic
toDynamic (Value _ x) = Just x
toDynamic (e1 :$ e2)  = do v1 <- toDynamic e1
                           v2 <- toDynamic e2
                           dynApply v1 v2

instance Show Expr where
  showsPrec d e = showParen (d > 10)
                $ showsPrecExpr 0 e
                . showString " :: "
                . showsTypeExpr e

showsTypeExpr :: Expr -> String -> String
showsTypeExpr e = case etyp e of
  Left (t1,t2) -> showString "ill-typed # "
                . shows t1
                . showString " $ "
                . shows t2
                . showString " #"
  Right t -> shows t

showsPrecExpr :: Int -> Expr -> String -> String
showsPrecExpr d (Value "_" _)     = showString "_" -- a hole
showsPrecExpr d (Value ('_':s) _) = showParen (isInfix s) $ showString s -- a variable
showsPrecExpr d (Value s _) | isInfixedPrefix s = showString $ toPrefix s
showsPrecExpr d (Value s _) | isNegativeLiteral s = showParen (d > 0) $ showString s
showsPrecExpr d (Value s _) = showParen sp $ showString s
  where sp = if atomic s then isInfix s else maybe True (d >) $ outernmostPrec s
showsPrecExpr d ((Value ":" _ :$ e1) :$ e2)
  | isConst e1 && mtyp e1 == Just (typeOf (undefined :: Char)) =
  case showsTailExpr e2 "" of
    '\"':cs  -> showString ("\"" ++ (init . tail) (showsPrecExpr 0 e1 "") ++ cs)
    cs -> showParen (d > prec ":")
        $ showsOpExpr ":" e1 . showString ":" . showString cs
showsPrecExpr d ((Value ":" _ :$ e1) :$ e2) =
  case showsTailExpr e2 "" of
    "[]" -> showString "[" . showsPrecExpr 0 e1 . showString "]"
    '[':cs -> showString "[" . showsPrecExpr 0 e1 . showString "," . showString cs
    cs -> showParen (d > prec ":")
        $ showsOpExpr ":" e1 . showString ":" . showString cs
showsPrecExpr d ee | isTuple ee = id
    showString "("
  . foldr1 (\s1 s2 -> s1 . showString "," . s2)
           (showsPrecExpr 0 `map` unfoldTuple ee)
  . showString ")"
showsPrecExpr d ((Value f _ :$ e1) :$ e2)
  | isInfix f = showParen (d > prec f)
              $ showsOpExpr f e1
              . showString " " . showString f . showString " "
              . showsOpExpr f e2
  | otherwise = showParen (d > prec " ")
              $ showString f
              . showString " " . showsOpExpr " " e1
              . showString " " . showsOpExpr " " e2
showsPrecExpr d (Value f _ :$ e1)
  | isInfix f = showParen True
              $ showsOpExpr f e1 . showString " " . showString f
showsPrecExpr d (e1 :$ e2) = showParen (d > prec " ")
                           $ showsPrecExpr (prec " ") e1
                           . showString " "
                           . showsPrecExpr (prec " " + 1) e2
-- bad smell here, repeated code!
showsTailExpr :: Expr -> String -> String
showsTailExpr ((Value ":" _ :$ e1) :$ e2)
  | isConst e1 && mtyp e1 == Just (typeOf (undefined :: Char)) =
  case showsPrecExpr 0 e2 "" of
    '\"':cs  -> showString ("\"" ++ (init . tail) (showsPrecExpr 0 e1 "") ++ cs)
    cs -> showsOpExpr ":" e1 . showString ":" . showsTailExpr e2
showsTailExpr ((Value ":" _ :$ e1) :$ e2) =
  case showsPrecExpr 0 e2 "" of
    "[]" -> showString "[" . showsPrecExpr 0 e1 . showString "]"
    '[':cs -> showString "[" . showsPrecExpr 0 e1 . showString "," . showString cs
    cs -> showsOpExpr ":" e1 . showString ":" . showsTailExpr e2
showsTailExpr e = showsOpExpr ":" e

showsOpExpr :: String -> Expr -> String -> String
showsOpExpr op = showsPrecExpr (prec op + 1)

showOpExpr :: String -> Expr -> String
showOpExpr op = showPrecExpr (prec op + 1)

showPrecExpr :: Int -> Expr -> String
showPrecExpr n e = showsPrecExpr n e ""

showExpr :: Expr -> String
showExpr = showPrecExpr 0
-- TODO: document and test showExpr

-- | Does not evaluate values when comparing, but rather uses their
--   representation as strings and their types.
instance Eq Expr where
  Value s1 d1 == Value s2 d2  =  dynTypeRep d1 == dynTypeRep d2 && s1 == s2
  ef1 :$ ex1  == ef2 :$ ex2   =  ef1 == ef2 && ex1 == ex2
  _           == _            =  False

instance Ord Expr where
  compare = compareComplexity <> lexicompare

-- | /O(n)/.
-- Compares the complexity of two 'Expr's.
-- An expression /e1/ is /strictly simpler/ than another expression /e2/
-- if the first of the following conditions to distingish between them is:
--
-- 1. /e1/ is smaller in size\/length than /e2/,
--    e.g.: @x + y < x + (y + z)@;
--
-- 2. or, /e1/ has more distinct variables than /e2/,
--    e.g.: @x + y < x + x@;
--
-- 3. or, /e1/ has more variable occurrences than /e2/,
--    e.g.: @x + x < 1 + x@;
--
-- 4. or, /e1/ has fewer distinct constants than /e2/,
--    e.g.: @1 + 1 < 0 + 1@.
--
-- They're otherwise considered of equal complexity,
-- e.g.: @x + y@ and @y + z@.
--
-- > > (xx -+- yy) `compareComplexity` (xx -+- (yy -+- zz))
-- > LT
--
-- > > (xx -+- yy) `compareComplexity` (xx -+- xx)
-- > LT
--
-- > > (xx -+- xx) `compareComplexity` (one -+- xx)
-- > LT
--
-- > > (one -+- one) `compareComplexity` (zero -+- one)
-- > LT
--
-- > > (xx -+- yy) `compareComplexity` (yy -+- zz)
-- > EQ
--
-- > > (zero -+- one) `compareComplexity` (one -+- zero)
-- > EQ
compareComplexity :: Expr -> Expr -> Ordering
compareComplexity  =  (compare      `on` length . values)
                   <> (flip compare `on` length . nubVars)
                   <> (flip compare `on` length . vars)
                   <> (compare      `on` length . nubConsts)

lexicompareBy :: (Expr -> Expr -> Ordering) -> Expr -> Expr -> Ordering
lexicompareBy compareConstants = cmp
  where
  e1@(Value ('_':s1) _) `cmp` e2@(Value ('_':s2) _)  =  typ e1 `compareTy` typ e2
                                                     <> s1 `compare` s2
  (f :$ x)        `cmp` (g :$ y)                     =  f  `cmp` g <> x `cmp` y
  (_ :$ _)        `cmp` _                            =  GT
  _               `cmp` (_ :$ _)                     =  LT
  _               `cmp` Value ('_':_) _              =  GT
  Value ('_':_) _ `cmp` _                            =  LT
  e1@(Value _ _)        `cmp` e2@(Value _ _)         =  e1 `compareConstants` e2
  -- Var < Constants < Apps

lexicompareConstants :: Expr -> Expr -> Ordering
lexicompareConstants = cmp
  where
  e1 `cmp` e2 | typ e1 /= typ e2 = typ e1 `compareTy` typ e2
  Value s1 _ `cmp` Value s2 _ = s1 `compare` s2
  _ `cmp` _ = error "lexicompareConstants can only compare constants"

lexicompare :: Expr -> Expr -> Ordering
lexicompare = lexicompareBy lexicompareConstants

-- | /O(n)/.
-- Unfold a function application 'Expr' into a list of function and
-- arguments.
--
-- > unfoldApp e0                          =  [e0]
-- > unfoldApp (e0 :$ e1)                  =  [e0,e1]
-- > unfoldApp ((e0 :$ e1) :$ e2)          =  [e0,e1,e2]
-- > unfoldApp (((e0 :$ e1) :$ e2) :$ e3)  =  [e0,e1,e2,e3]
--
-- > unfoldApp $ e0                    =  [e0]
-- > unfoldApp $ e0 :$ e1              =  [e0,e1]
-- > unfoldApp $ e0 :$ e1 :$ e2        =  [e0,e1,e2]
-- > unfoldApp $ e0 :$ e1 :$ e2 :$ e3  =  [e0,e1,e2,e3]
unfoldApp :: Expr -> [Expr]
unfoldApp e  =  u e []
  where
  u (ef :$ ex) = u ef . (ex:)
  u ex         = (ex:)

-- | /O(n)/.
-- Unfold a tuple 'Expr' into a list of values.
--
-- > > let pair' a b = value "," ((,) :: Bool->Char->(Bool,Char)) :$ a :$ b
--
-- > > pair' (val True) (val 'a')
-- > (True,'a') :: (Bool,Char)
--
-- > > unfoldTuple $ pair' (val True) (val 'a')
-- > [True :: Bool,'a' :: Char]
--
-- > > let trio' a b c = value ",," ((,,) :: Bool->Char->Int->(Bool,Char,Int)) :$ a :$ b :$ c
--
-- > > trio' (val False) (val 'b') (val (9 :: Int))
-- > (False,'b',9) :: (Bool,Char,Int)
--
-- > > unfoldTuple $ trio' (val False) (val 'b') (val (9 :: Int))
-- > [False :: Bool,'b' :: Char,9 :: Int]
unfoldTuple :: Expr -> [Expr]
unfoldTuple = u . unfoldApp
  where
  u (Value cs _:es) | not (null es) && cs == replicate (length es - 1) ',' = es
  u _   = [] -- TODO: only return an empty list for units
-- NOTE: the above function does not work when the representation of the
--       tupling function is (,) or (,,) or (,,,) or ...
--       This is intentional to allow the 'Show' 'Expr' instance
--       to present (,,) 1 2 differently than (1,2).

isTuple :: Expr -> Bool
isTuple = not . null . unfoldTuple

-- | /O(n)/.
-- Check if an 'Expr' has a variable.  (By convention, any value whose
-- 'String' representation starts with @'_'@.)
--
-- > > hasVar $ value "not" not :$ val True
-- > False
--
-- > > hasVar $ value "&&" (&&) :$ var "p" (undefined :: Bool) :$ val True
-- > True
hasVar :: Expr -> Bool
hasVar (e1 :$ e2) = hasVar e1 || hasVar e2
hasVar (Value ('_':_) _) = True
hasVar _ = False

-- | /O(n)/.
-- Returns whether a 'Expr' has /no/ variables.
-- This is equivalent to "@not . hasVar@".
--
-- The name "ground" comes from term rewriting.
--
-- > > isGround $ value "not" not :$ val True
-- > True
--
-- > > isGround $ value "&&" (&&) :$ var "p" (undefined :: Bool) :$ val True
-- > False
isGround :: Expr -> Bool
isGround  =  not . hasVar

-- | /O(1)/.
-- Returns whether an 'Expr' is a terminal constant.
-- (cf. 'isGround').
--
-- > > isConst $ var "x" (undefined :: Int)
-- > False
--
-- > > isConst $ val False
-- > True
--
-- > > isConst $ value "not" not :$ val False
-- > False
isConst :: Expr -> Bool
isConst  (Value ('_':_) _)  =  False
isConst  (Value _ _)        =  True
isConst  _                  =  False

-- | /O(1)/.
-- Returns whether an 'Expr' is a terminal variable.
-- (cf. 'hasVar').
--
-- > > isVar $ var "x" (undefined :: Int)
-- > True
--
-- > > isVar $ val False
-- > False
--
-- > > isVar $ value "not" not :$ var "p" (undefined :: Bool)
-- > False
isVar :: Expr -> Bool
isVar (Value ('_':_) _)  =  True
isVar _                  =  False

subexprs :: Expr -> [Expr]
subexprs e  =  s e []
  where
  s :: Expr -> [Expr] -> [Expr]
  s e@(e1 :$ e2)  =  (e:) . s e1 . s e2
  s e             =  (e:)
-- TODO: document & test subexprs

nubSubexprs :: Expr -> [Expr]
nubSubexprs  =  nubSort . subexprs
-- TODO: document & test nubSubexprs

-- | /O(n)/.
-- Lists all terminal values in an expression in order and with repetitions.
-- (cf. 'nubValues')
--
-- > > values (xx -+- yy)
-- > [ (+) :: Int -> Int -> Int
-- > , x :: Int
-- > , y :: Int ]
--
-- > > values (xx -+- (yy -+- zz))
-- > [ (+) :: Int -> Int -> Int
-- > , x :: Int
-- > , (+) :: Int -> Int -> Int
-- > , y :: Int
-- > , z :: Int ]
--
-- > > values (zero -+- (one -*- two))
-- > [ (+) :: Int -> Int -> Int
-- > , 0 :: Int
-- > , (*) :: Int -> Int -> Int
-- > , 1 :: Int
-- > , 2 :: Int ]
--
-- > > values (pp -&&- trueE)
-- > [ (&&) :: Bool -> Bool -> Bool
-- > , p :: Bool
-- > , True :: Bool ]
values :: Expr -> [Expr]
values e  =  v e []
  where
  v :: Expr -> [Expr] -> [Expr]
  v (e1 :$ e2)  =  v e1 . v e2
  v e           =  (e:)

-- | /O(n log n)/.
-- Lists all terminal values in an expression without repetitions.
-- (cf. 'values')
--
-- > > nubValues (xx -+- yy)
-- > [ x :: Int
-- > , y :: Int
-- > , (+) :: Int -> Int -> Int ]
--
-- > > nubValues (xx -+- (yy -+- zz))
-- > [ x :: Int
-- > , y :: Int
-- > , z :: Int
-- > , (+) :: Int -> Int -> Int ]
--
-- > > nubValues (zero -+- (one -*- two))
-- > [ 0 :: Int
-- > , 1 :: Int
-- > , 2 :: Int
-- > , (*) :: Int -> Int -> Int
-- > , (+) :: Int -> Int -> Int ]
--
-- > > nubValues (pp -&&- pp)
-- > [ p :: Bool
-- > , (&&) :: Bool -> Bool -> Bool ]
nubValues :: Expr -> [Expr]
nubValues  =  nubSort . values

-- | /O(n)/.
-- List terminal constants in an expression in order and with repetitions.
-- (cf. 'nubConsts')
--
-- > > consts (xx -+- yy)
-- > [ (+) :: Int -> Int -> Int ]
--
-- > > consts (xx -+- (yy -+- zz))
-- > [ (+) :: Int -> Int -> Int
-- > , (+) :: Int -> Int -> Int ]
--
-- > > consts (zero -+- (one -*- two))
-- > [ (+) :: Int -> Int -> Int
-- > , 0 :: Int
-- > , (*) :: Int -> Int -> Int
-- > , 1 :: Int
-- > , 2 :: Int ]
--
-- > > consts (pp -&&- trueE)
-- > [ (&&) :: Bool -> Bool -> Bool
-- > , True :: Bool ]
consts :: Expr -> [Expr]
consts  =  filter isConst . values

-- | /O(n log n)/.
-- List terminal constants in an expression without repetitions.
-- (cf. 'consts')
--
-- > > nubConsts (xx -+- yy)
-- > [ (+) :: Int -> Int -> Int ]
--
-- > > nubConsts (xx -+- (yy -+- zz))
-- > [ (+) :: Int -> Int -> Int ]
--
-- > > nubConsts (pp -&&- trueE)
-- > [ True :: Bool
-- > , (&&) :: Bool -> Bool -> Bool ]
nubConsts :: Expr -> [Expr]
nubConsts  =  nubSort . consts

-- | /O(n)/.
-- Lists all variables in an expression in order and with repetitions.
-- (cf. 'nubVars')
--
-- > > vars (xx -+- yy)
-- > [ x :: Int
-- > , y :: Int ]
--
-- > > vars (xx -+- (yy -+- xx))
-- > [ x :: Int
-- > , y :: Int
-- > , x :: Int ]
--
-- > > vars (zero -+- (one -*- two))
-- > []
--
-- > > vars (pp -&&- trueE)
-- > [p :: Bool]
vars :: Expr -> [Expr]
vars  =  filter isVar . values

-- | /O(n log n)/.
-- Lists all variables in an expression without repetitions.
-- (cf. 'vars')
--
-- > > nubVars (yy -+- xx)
-- > [ x :: Int
-- > , y :: Int ]
--
-- > > nubVars (xx -+- (yy -+- xx))
-- > [ x :: Int
-- > , y :: Int ]
--
-- > > nubVars (zero -+- (one -*- two))
-- > []
--
-- > > nubVars (pp -&&- trueE)
-- > [p :: Bool]
nubVars :: Expr -> [Expr]
nubVars  =  nubSort . vars

-- | /O(n)/.
-- Return the arity of the given expression.
--
-- > > arity (val 0)
-- > 0
--
-- > > arity (val False)
-- > 0
--
-- > > arity (value "id" (id :: Int -> Int))
-- > 1
--
-- > > arity (value "const" (const :: Int -> Int -> Int))
-- > 2
--
-- > > arity (one -+- two)
-- > 0
arity :: Expr -> Int
arity  =  tyArity . typ

size :: Expr -> Int
size  =  length . values
-- TODO: document & test size

depth :: Expr -> Int
depth e@(_:$_)  =  1 + maximum (map depth $ unfoldApp e)
depth _         =  1
-- TODO: document & test depth
-- TODO: possibly rename depth
-- TODO: add alternative depth function which does not use unfoldApp and yield
--       different results
