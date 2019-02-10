-- |
-- Module      : Data.Haexpress.Core
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Expr' type and utilities involving it
{-# LANGUAGE CPP, DeriveDataTypeable #-} -- for GHC < 7.10
module Data.Haexpress.Core
  ( Expr (..)
  , value
  , val
  , var
  , varAsTypeOf
  , hole
  , evaluate
  , eval
  , toDynamic
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
data Expr  =  Value String Dynamic
           |  Expr :$ Expr
  deriving Typeable -- for GHC < 7.10

-- | It takes a string representation of a value and a value, returning an
--   'Expr' with that terminal value.  Examples:
--
-- > value "0" 0
-- > value "'a'" 'a'
-- > value "True" True
-- > value "id" (id :: Int -> Int)
-- > value "(+)" ((+) :: Int -> Int -> Int)
-- > value "sort" (sort :: [Bool] -> [Bool])
value :: Typeable a => String -> a -> Expr
value s x = Value s (toDyn x)

-- | A shorthand for 'value' for values that are 'Show' instances.
--
-- > val 0     =  value "0" 0
-- > val 'a'   =  value "'a'" 'a' 
-- > val True  =  value "True" True
val :: (Typeable a, Show a) => a -> Expr
val x = Value (show x) (toDyn x)

-- | @var "x" (undefined :: Ty)@ returns a variable of type @Ty@ named "x"
var :: Typeable a => String -> a -> Expr
var s a = value ('_':s) (undefined `asTypeOf` a)

-- | A typed hole.
hole :: Typeable a => a -> Expr
hole a = var "" (undefined `asTypeOf` a)

-- | Creates a 'var'iable with the same type as the given 'Expr'.
--
-- > > let one = val (1::Int)
-- > > "x" `varAsTypeOf` one
-- > x :: Int
--
-- You should use 'var' instead if you are creating values directly from types:
--
-- > > "x" `varAsTypeOf` (val (undefined :: Int))
-- > x :: Int
-- > > "c" `varAsTypeOf` (val (undefined :: Char))
-- > c :: Char
--
-- You should consider using 'var' instead of this.
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

-- | The type of an expression.  This raises errors, but those should not
--   happen if expressions are smart-constructed.
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
evaluate :: Typeable a => Expr -> Maybe a
evaluate e = toDynamic e >>= fromDynamic

toDynamic :: Expr -> Maybe Dynamic
toDynamic (Value _ x) = Just x
toDynamic (e1 :$ e2)  = do v1 <- toDynamic e1
                           v2 <- toDynamic e2
                           dynApply v1 v2

-- | Evaluates an expression when possible (correct type, no holes).
--   Returns a default value otherwise.
eval :: Typeable a => a -> Expr -> a
eval x e = fromMaybe x (evaluate e)

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
-- TODO: the above show instance is getting big.  Move it into a separate file?

isTuple :: Expr -> Bool
isTuple = not . null . unfoldTuple

unfoldTuple :: Expr -> [Expr]
unfoldTuple = u . unfoldApp
  where
  u (Value cs _:es) | not (null es) && cs == replicate (length es - 1) ','
                       = es
  u _   = []

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

-- | Unfold function application:
--
-- > (((f :$ e1) :$ e2) :$ e3) = [f,e1,e2,e3]
unfoldApp :: Expr -> [Expr]
unfoldApp (ef :$ ex) = unfoldApp ef ++ [ex]
unfoldApp  ef        = [ef]
