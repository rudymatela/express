-- |
-- Module      : Data.Haexpress.Core
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Expr' type and utilities involving it
{-# LANGUAGE CPP, DeriveDataTypeable #-} -- for GHC < 7.10
module Data.Haexpress.Core
  (
  -- * The Expr datatype
    Expr (..)
  -- * Smart constructors
  , value
  , val
  , ($$)
  , var
  , hole
  -- * Evaluating Exprs
  , evaluate
  , eval
  , typ
  , toDynamic
  -- * Utilities
  , unfoldApp
  , hasVar
  , isGround
  , varAsTypeOf
  )
where

-- TODO: talk about convertion of preceding variables with "_"
-- TODO: document all functions with examples
-- TODO: more exports

import Data.Dynamic
import Data.Typeable (TypeRep, typeOf, funResultTy)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Haexpress.Utils.String

-- | A functional-application expression representation
data Expr  =  Value String Dynamic -- ^ a 'value' enconded as 'String' and 'Dynamic'
           |  Expr :$ Expr         -- ^ function application between expressions
  deriving Typeable -- for GHC < 7.10

-- | It takes a string representation of a value and a value, returning an
--   'Expr' with that terminal value.  Examples:
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

-- | A shorthand for 'value' for values that are 'Show' instances.
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

-- | Creates an 'Expr' representing a function application.
--   'Just' an 'Expr' application if the types match, 'Nothing' otherwise.
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
e1 $$ e2  =  case typ e1 `funResultTy` typ e2 of
             Nothing -> Nothing
             Just _  -> Just $ e1 :$ e2

-- | Creates an 'Expr' representing a variable with the given name and argument
--   type.
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

-- | Creates an 'Expr' representing a typed hole of the given argument type.
--
-- > > hole (undefined :: Int)
-- > _ :: Int
--
-- > > hole (undefined :: Maybe String)
-- > _ :: Maybe [Char]
hole :: Typeable a => a -> Expr
hole a = var "" (undefined `asTypeOf` a)

-- | The type of an expression.  This raises errors, but those should not
--   happen if expressions are smart-constructed.
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
-- > *** Exception: type mismatch, cannot apply Int -> Int to Char
typ :: Expr -> TypeRep
typ (Value _ d) = dynTypeRep d
typ (e1 :$ e2) =
  case typ e1 `funResultTy` typ e2 of
    Nothing -> error $ "type mismatch, cannot apply "
                    ++ show (typ e1) ++ " to " ++ show (typ e2)
    Just t  -> t
-- TODO: also provide a Expr -> Either String TypeRep
-- TODO: also provide a Expr -> Either (TypeRep, TypeRep) TypeRep
-- TODO: also provide a fastType which ignores type mismatches

-- | 'Just' the value of an expression when possible (correct type),
--   'Nothing' otherwise.
--   This does not catch errors from 'undefined' 'Dynamic' 'value's.
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

-- | Evaluates an expression when possible (correct type).
--   Returns a default value otherwise.
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

-- | Evaluates an expression to a terminal 'Dynamic' value when possible.
--   Returns 'Nothing' otherwise.
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

-- TODO: decide whether to always show the type
-- TODO: decide whether to always show holes
instance Show Expr where
  showsPrec d e = showParen (d > 10)
                $ showsPrecExpr 0 e
                . showString " :: "
                . shows (typ e)
--              . showString (showHoles e)  -- TODO:
--  where
--  showHoles e = case holes e of
--                  [] -> ""
--                  hs -> "  (holes: " ++ intercalate ", " (map show hs) ++ ")"

showsPrecExpr :: Int -> Expr -> String -> String
showsPrecExpr d (Value "_" _)     = showString "_" -- a hole
showsPrecExpr d (Value ('_':s) _) = showParen (isInfix s) $ showString s -- a variable
showsPrecExpr d (Value s _) | isInfixedPrefix s = showString $ toPrefix s
showsPrecExpr d (Value s _) | isNegativeLiteral s = showParen (d > 0) $ showString s
showsPrecExpr d (Value s _) = showParen sp $ showString s
  where sp = if atomic s then isInfix s else maybe True (d >) $ outernmostPrec s
showsPrecExpr d ((Value ":" _ :$ e1@(Value _ _)) :$ e2)
  | typ e1 == typeOf (undefined :: Char) =
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
showsTailExpr ((Value ":" _ :$ e1@(Value _ _)) :$ e2)
  | typ e1 == typeOf (undefined :: Char) =
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

-- | Does not evaluate values when comparing, but rather uses their
--   representation as strings and their types.
instance Eq Expr where
  Value s1 d1 == Value s2 d2  =  dynTypeRep d1 == dynTypeRep d2 && s1 == s2
  ef1 :$ ex1  == ef2 :$ ex2   =  ef1 == ef2 && ex1 == ex2
  _           == _            =  False

-- TODO: the above show instance is getting big.  Move it into a separate file?

-- TODO: isList
-- TODO: unfoldList
-- TODO: isGround

-- | Unfold a function application 'Expr' into a list of function and
--   arguments.
--
-- > e0                    =    e0                      =  [e0]
-- > e0 :$ e1              =    e0 :$ e1                =  [e0,e1]
-- > e0 :$ e1 :$ e2        =   (e0 :$ e1) :$ e2         =  [e0,e1,e2]
-- > e0 :$ e1 :$ e2 :$ e3  =  ((e0 :$ e1) :$ e2) :$ e3  =  [e0,e1,e2,e3]
unfoldApp :: Expr -> [Expr]
unfoldApp (ef :$ ex) = unfoldApp ef ++ [ex]
unfoldApp  ef        = [ef]

-- | Unfold a tuple 'Expr' into a list of values.
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

-- | Check if an 'Expr' has a variable.  (By convention, any value whose
--   'String' representation starts with @'_'@.)
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

-- | Returns whether a 'Expr' has _no_ variables.
--   This is equivalent to @not . hasVar@.
--
--   The name "ground" comes from term rewriting.
--
-- > > isGround $ value "not" not :$ val True
-- > True
--
-- > > isGround $ value "&&" (&&) :$ var "p" (undefined :: Bool) :$ val True
-- > False
isGround :: Expr -> Bool
isGround = not . hasVar

-- | Creates a 'var'iable with the same type as the given 'Expr'.
--
-- > > let one = val (1::Int)
-- > > "x" `varAsTypeOf` one
-- > x :: Int
varAsTypeOf :: String -> Expr -> Expr
varAsTypeOf n = Value ('_':n) . undefine . fromMaybe err . toDynamic
  where
  err = error "varAsTypeOf: could not compile Dynamic value, type error?"
  undefine :: Dynamic -> Dynamic
#if __GLASGOW_HASKELL__ >= 806
  undefine (Dynamic t v) = (Dynamic t undefined)
#else
  undefine = id -- there's no way to do this using the old Data.Dynamic API.
#endif
