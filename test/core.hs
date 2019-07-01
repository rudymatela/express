-- Copyright (c) 2017-2018 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Haexpress.Utils.List
import Test.LeanCheck.Error (errorToNothing)

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  -- smart constructors and evaluation

  , holds n $ \x -> eval (undefined :: Int -> Int) (value "abs" (abs :: Int -> Int)) x == abs (x :: Int)
  , evl (val (10 :: Int)) == (10 :: Int)
  , evl (val (1337 :: Int)) == (1337 :: Int)
  , evl (val False) == False
  , holds n $ \x y -> evl (value "+" ((+) :: Int -> Int -> Int) :$ val x :$ val y) == (x + y :: Int)
  , holds n $ \x y -> evl (value "+" ((*) :: Int -> Int -> Int) :$ val x :$ val y) == (x * y :: Int)
  , holds n $ \i -> evl (val i) == (i :: Int)
  , show (one -+- one) == "1 + 1 :: Int"
  , show absE == "abs :: Int -> Int"
  , show notE == "not :: Bool -> Bool"
  , show andE == "(&&) :: Bool -> Bool -> Bool"
  , show (pp -&&- (not' false)) == "p && not False :: Bool"
  , show (one :$ one) == "1 1 :: ill-typed # Int $ Int #"
  , holds n $ \(IntE xx, IntE yy) -> isJust (toDynamic $ xx -+- yy)
  , holds n $ \(IntE xx, IntE yy) -> isGround xx && isGround yy
                                 ==> evl (xx -+- yy) =$ errorToNothing $= (evl (yy -+- xx) :: Int)

  -- valid applications
  , holds n $ \(IntToIntE   ef) (IntE  ex) -> isJust (ef $$ ex)
  , holds n $ \(BoolToBoolE ef) (BoolE ep) -> isJust (ef $$ ep)

  -- invalid applications
  , holds n $ \(IntE ex)  (IntE  ey) -> isNothing (ex $$ ey)
  , holds n $ \(BoolE ep) (BoolE eq) -> isNothing (ep $$ eq)
  , holds n $ \(BoolToBoolE ef) (IntE  ex) -> isNothing (ef $$ ex)
  , holds n $ \(IntToIntE   ef) (BoolE ep) -> isNothing (ef $$ ep)
  , holds n $ \(IntE ex) (IntE ey) (IntE ez) -> isNothing (ex $$ (ey :$ ez))
  , holds n $ \(IntE ex) (IntE ey) (IntE ez) -> isNothing ((ex :$ ey) $$ ez)


  -- typing
  , typ zero       == tyInt
  , typ one        == tyInt
  , typ xx         == tyInt
  , typ bee        == tyChar
  , typ xxs        == tyLInt
  , typ (ff xx)    == tyInt
  , typ (abs' one) == tyInt
  , typ true       == tyBool
  , typ pp         == tyBool

  , etyp zero       == Right tyInt
  , etyp (abs' one) == Right tyInt
  , etyp (abs' bee) == Left (tyIntToInt, tyChar)
  , etyp (abs' bee :$ zero) == Left (tyIntToInt, tyChar)
  , etyp ((zero :$ one) :$ (bee :$ cee)) == Left (tyInt, tyInt)

  , isIllTyped (abs' zero) == False
  , isIllTyped (zero :$ one) == True
  , isWellTyped (abs' zero) == True
  , isWellTyped (zero :$ one) == False

  -- our Listable Expr enumeration does not produce ill typed Exprs
  , holds n $ isRight . etyp
  , holds n $ isJust  . mtyp
  , holds n $ isWellTyped
  , holds n $ not . isIllTyped

  -- our Listable Ill enumeration only produces ill typed Exprs
  , holds n $ isLeft    . etyp . unIll
  , holds n $ isNothing . mtyp . unIll
  , holds n $ isIllTyped . unIll
  , holds n $ not . isWellTyped . unIll

  -- we don't need the precondition here given the above
  -- but it's added just in case
  , holds n $ \e -> isRight (etyp e) ==> etyp e == Right (typ e)
  , holds n $ \e -> isJust  (mtyp e) ==> mtyp e == Just  (typ e)

  -- we prefer returning errors to the left
  , holds n $ \(Ill ef) (Ill ex) -> etyp (ef :$ ex) == etyp ef
  , holds n $ \ef (Ill ex) -> etyp (ef :$ ex) == etyp ex


  -- boolean properties

  , hasVar (zero -+- one) == False
  , hasVar (xx -+- yy) == True

  , isGround (zero -+- (one -*- two)) == True
  , isGround (xx -+- (one -*- three)) == False

  , holds n $ isGround === not . hasVar

  , isVar xx == True
  , isVar yy == True
  , isVar ffE == True
  , isVar (xx -+- yy) == False
  , isVar (ff xx) == False
  , isVar one == False
  , isVar (one -+- two) == False

  , isHole i_ == True
  , isHole b_ == True
  , isHole xx == False

  , isConst xx == False
  , isConst yy == False
  , isConst (xx -+- yy) == False
  , isConst (ff xx) == False
  , isConst one == True
  , isConst two == True
  , isConst absE == True
  , isConst (one -+- two) == False

  , values (xx -+- yy) == [plusE, xx, yy]
  , values (xx -+- (yy -+- zz)) == [plusE, xx, plusE, yy, zz]
  , values ((xx -+- yy) -+- zz) == [plusE, plusE, xx, yy, zz]
  , values (zero -+- (one -*- two)) == [plusE, zero, timesE, one, two]
  , values (pp -&&- true) == [andE, pp, true]

  , subexprs (xx -+- yy) ==
      [ xx -+- yy
      , plusE :$ xx
      , plusE
      , xx
      , yy
      ]
  , subexprs (pp -&&- (pp -&&- true)) ==
      [ pp -&&- (pp -&&- true)
      , andE :$ pp
      , andE
      , pp
      , pp -&&- true
      , andE :$ pp
      , andE
      , pp
      , true
      ]
  , nubSubexprs (xx -+- yy) ==
      [ xx
      , yy
      , plusE
      , plusE :$ xx
      , xx -+- yy
      ]
  , nubSubexprs (pp -&&- (pp -&&- true)) ==
      [ pp
      , true
      , andE
      , andE :$ pp
      , pp -&&- true
      , pp -&&- (pp -&&- true)
      ]

  , holds n $ (okEqOrd :: Expr -> Expr -> Expr -> Bool)
  , holds n $ \(Ill e0) (Ill e1) (Ill e2) -> okEqOrd e0 e1 e2


  -- boolean properties
  , holds n $ \e -> isHole e ==> isVar e

  -- listing subexpressions
  , holds n $ \e -> isGround e ==> consts e == values e

  , holds n $ \e -> nubSubexprs e `isSubsetOf` subexprs e
  , holds n $ \e -> nubValues e `isSubsetOf` values e
  , holds n $ \e -> nubVars   e `isSubsetOf` vars   e
  , holds n $ \e -> nubConsts e `isSubsetOf` consts e
  , holds n $ \e -> values e    `isSubsetOf` subexprs e
  , holds n $ \e -> vars      e `isSubsetOf` values e
  , holds n $ \e -> consts    e `isSubsetOf` values e
  , holds n $ \e -> (vars e ++ consts e) `isPermutationOf` values e
  , holds n $ \e -> (nubVars e ++ nubConsts e) `isPermutationOf` nubValues e


  -- showing expressions

  , show zero == "0 :: Int"
  , show two == "2 :: Int"
  , show minusOne == "-1 :: Int"
  , show (one -+- two -*- three)   == "1 + 2 * 3 :: Int"
  , show ((one -+- two) -*- three) == "(1 + 2) * 3 :: Int"

  , show plusE == "(+) :: Int -> Int -> Int"
  , show timesE == "(*) :: Int -> Int -> Int"

  , show (plusE :$ one) == "(1 +) :: Int -> Int"
  , show (timesE :$ (minusOne -+- two)) == "(((-1) + 2) *) :: Int -> Int"

  -- TODO: make the following work
  -- (It didn't work on Speculate anyway...)
--, show (var "`f`" (undefined :: Int -> Int -> Int) :$ one) == "(1 `f`) :: Int" -- TODO:
--, show (var "`f`" (undefined :: Int -> Int -> Int) :$ one :$ two) == "1 `f` 2 :: Int" -- TODO:
  , show (value "`compare`" (compare :: Int->Int->Ordering) :$ one) == "(1 `compare`) :: Int -> Ordering"
  , show (value "`compare`" (compare :: Int->Int->Ordering) :$ one :$ two) == "1 `compare` 2 :: Ordering"

  , holds n $ show . mapVars (\(Value ('_':s) d) -> Value (if null s then "_" else s) d) === show
--, holds n $ show . mapConsts (\(Value s d) -> Value ('_':s) d) === show -- TODO:

  , show (emptyStringE) == "\"\" :: [Char]"
  , show (spaceE -:- emptyStringE) == "\" \" :: [Char]"
  , show (spaceE -:- ccs)          == "' ':cs :: [Char]"
  , show (ae -:- bee -:- emptyStringE) == "\"ab\" :: [Char]"
  , show (ae -:- bee -:- ccs)          == "'a':'b':cs :: [Char]"
  , show (ae -:- spaceE -:- bee -:- lineBreakE -:- emptyStringE) == "\"a b\\n\" :: [Char]"
  , show (cc -:- spaceE -:- dd -:- lineBreakE -:- emptyStringE) == "c:' ':d:\"\\n\" :: [Char]"
  , show (cc -:- spaceE -:- dd -:- lineBreakE -:- ccs)          == "c:' ':d:'\\n':cs :: [Char]"
  , show (cc -:- ae -:- bee -:- emptyStringE) == "c:\"ab\" :: [Char]"
  , show (cc -:- ae -:- bee -:- spaceE -:- ae -:- bee -:- emptyStringE) == "c:\"ab ab\" :: [Char]"

  , show one                     == "1 :: Int"
  , show (minusOne)              == "-1 :: Int"
  , show (one -+- one)           == "1 + 1 :: Int"
  , show (minusOne -+- minusOne) == "(-1) + (-1) :: Int"

  , show (zero -|- one)          == "(0,1) :: (Int,Int)"
  , show (minusOne -|- minusOne) == "(-1,-1) :: (Int,Int)"
  , show (triple zero one two)   == "(0,1,2) :: (Int,Int,Int)"
  , show (quadruple minusOne zero one two) == "(-1,0,1,2) :: (Int,Int,Int,Int)"
  , show (quintuple minusOne zero one two three) == "(-1,0,1,2,3) :: (Int,Int,Int,Int,Int)"
  , show (sixtuple minusTwo minusOne zero one two three) == "(-2,-1,0,1,2,3) :: (Int,Int,Int,Int,Int,Int)"

  , show (one -:- nilE)                    == "[1] :: [Int]"
  , show (zero -:- one -:- nilE)           == "[0,1] :: [Int]"
  , show (minusOne -:- nilE)               == "[-1] :: [Int]"
  , show (minusOne -:- minusTwo -:- nilE)  == "[-1,-2] :: [Int]"
  , show (xx -:- minusTwo -:- yy -:- nilE) == "[x,-2,y] :: [Int]"
  , show (xx -:- minusTwo -:- yy -:- xxs)  == "x:(-2):y:xs :: [Int]"

  , show (ffE -$- zero)     == "f $ 0 :: Int"
  , show (ggE -$- xx)       == "g $ x :: Int"
  , show (ffE -$- minusOne) == "f $ (-1) :: Int"

  , arity zero == 0
  , arity xx == 0
  , arity absE == 1
  , arity plusE == 2
  , arity timesE == 2

  , size zero == 1
  , size (one -+- two) == 3
  , size (abs' one) == 2

  , depth zero == 1
  , depth (one -+- two) == 2
  , depth (abs' one -+- two) == 3

  , height zero == 1
  , height (abs' one) == 2
  , height ((const' one) two) == 3
  , height ((const' (abs' one)) two) == 4
  , height ((const' one) (abs' two)) == 3

  , holds n $ \e -> depth e  <= height e
  , holds n $ \e -> depth e  <= size e
  , holds n $ \e -> height e <= size e
  ]
