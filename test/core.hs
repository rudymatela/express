-- Copyright (c) 2019-2021 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Express.Utils.List
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
  , typ xxs        == tyInts
  , typ (ff xx)    == tyInt
  , typ (abs' one) == tyInt
  , typ true       == tyBool
  , typ pp         == tyBool

  , etyp zero       == Right tyInt
  , etyp (abs' one) == Right tyInt
  , etyp (abs' bee) == Left (tyIntToInt, tyChar)
  , etyp (abs' bee :$ zero) == Left (tyIntToInt, tyChar)
  , etyp ((zero :$ one) :$ (bee :$ cee)) == Left (tyInt, tyInt)

  , etyp (xx :$ yy) == Left (tyInt, tyInt)
  , etyp (xx :$ (cc :$ yy)) == Left (tyChar, tyInt)
  , etyp (abs' xx :$ (ord' cc :$ negate' yy)) == Left (tyInt, tyInt)
  , holds n $ \(SameTypeE ef eg) (SameTypeE ex ey) -> (etyp (ef :$ ex) == etyp (eg :$ ey))
  , holds n $ \ef eg ex ey -> (etyp ef == etyp eg && etyp ex == etyp ey)
                           == (etyp (ef :$ ex) == etyp (eg :$ ey))

  , isIllTyped (abs' zero) == False
  , isIllTyped (zero :$ one) == True
  , isWellTyped (abs' zero) == True
  , isWellTyped (zero :$ one) == False

  , isFun (value "abs" (abs :: Int -> Int)) == True
  , isFun (val (1::Int)) == False
  , isFun (value "const" (const :: Bool -> Bool -> Bool) :$ val False) == True
  , holds n $ \e -> (arity e /= 0) == isFun e

  -- eq instance
  , xx -+- yy == xx -+- yy
  , xx -+- yy /= yy -+- xx


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

  -- isValue and isApp
  , holds n $ \e1 e2  ->  isValue (e1 :$ e2)  ==  False
  , holds n $ \e1 e2  ->  isApp   (e1 :$ e2)  ==  True
  , holds n $ isValue === not . isApp
  , holds n $ isApp   === not . isValue
  , holds n $ \e  ->  isValue e  ==  (isVar e || isConst e)
  , holds n $ \e  ->  isApp e    ==  (not (isVar e) && not (isConst e))

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

  , values (xx -+- yy) == [plus, xx, yy]
  , values (xx -+- (yy -+- zz)) == [plus, xx, plus, yy, zz]
  , values ((xx -+- yy) -+- zz) == [plus, plus, xx, yy, zz]
  , values (zero -+- (one -*- two)) == [plus, zero, times, one, two]
  , values (pp -&&- true) == [andE, pp, true]

  , subexprs (xx -+- yy) ==
      [ xx -+- yy
      , plus :$ xx
      , plus
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
      , plus
      , plus :$ xx
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

  -- in case implementation changes
  , holds n $ \e -> nubSubexprs e == nubSort (subexprs e)
  , holds n $ \e -> nubValues   e == nubSort (values e)
  , holds n $ \e -> nubVars     e == nubSort (vars e)
  , holds n $ \e -> nubConsts   e == nubSort (consts e)

  , arity zero == 0
  , arity xx == 0
  , arity absE == 1
  , arity plus == 2
  , arity times == 2

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

  , size  zero == 1
  , depth zero == 1
  , size  one  == 1
  , depth one  == 1
  , size  (zero -+- one) == 3
  , depth (zero -+- one) == 2
  , size  (zero -+- (xx -+- yy)) == 5
  , depth (zero -+- (xx -+- yy)) == 3
  , size  (((xx -+- yy) -*- zz) -==- ((xx -*- zz) -+- (yy -*- zz))) == 13
  , depth (((xx -+- yy) -*- zz) -==- ((xx -*- zz) -+- (yy -*- zz))) ==  4
  , depth (xx -*- yy -+- xx -*- zz -==- xx -*- (yy -+- zz)) == 4
  , size  (xx -*- yy -+- xx -*- zz -==- xx -*- (yy -+- zz)) == 13
  , depth (xx -*- yy -+- xx -*- zz) == 3
  , depth (xx -*- (yy -+- zz)) == 3

  , nubConsts (xx -+- yy) == [plus]
  , nubConsts (xx -+- (yy -+- zz)) == [plus]
  , nubConsts (zero -+- one) =$ sort $= [zero, one, plus]
  , nubConsts ((zero -+- abs' zero) -+- (ord' ae -+- ord' cc))
      =$ sort $= [zero, ae, absE, plus, ordE]
  , holds n $ \e1 e2 -> times `elem` consts (e1 -*- e2)

  , vars (xx -+- yy) == [xx, yy]
  , nubVars (xx -+- xx) == [xx]
  , nubVars (xx -+- xx -+- yy) == [xx, yy]
  , nubVars (yy -+- xx -+- yy) == [xx, yy]
  ]
