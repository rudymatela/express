-- Copyright (c) 2017-2018 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Haexpress.Utils.List

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True

  -- smart constructors and evaluation

  , holds n $ \x -> eval (undefined :: Int -> Int) (value "abs" (abs :: Int -> Int)) x == abs (x :: Int)
  , evalInt (val (10 :: Int)) == 10
  , holds n $ \x y -> evalInt (value "+" ((+) :: Int -> Int -> Int) :$ val x :$ val y) == x + y
  , holds n $ \x y -> evalInt (value "+" ((*) :: Int -> Int -> Int) :$ val x :$ val y) == x * y
  , holds n $ \i -> evalInt (val i) == i
  , show (one -+- one) == "1 + 1 :: Int"
  , holds n $ \(IntE xx, IntE yy) -> isJust (toDynamic $ xx -+- yy)
  , holds n $ \(IntE xx, IntE yy) -> isGround xx && isGround yy
                                 ==> evalInt (xx -+- yy) == evalInt (yy -+- xx)

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
  , typ xxss       == tyLInt
  , typ (ff xx)    == tyInt
  , typ (abs' one) == tyInt
  , typ trueE      == tyBool
  , typ pp         == tyBool

  , etyp zero       == Right tyInt
  , etyp (abs' one) == Right tyInt
  , etyp (abs' bee) == Left (tyIntToInt, tyChar)
  , etyp (abs' bee :$ zero) == Left (tyIntToInt, tyChar)
  , etyp ((zero :$ one) :$ (bee :$ cee)) == Left (tyInt, tyInt)

  , isIll (abs' zero) == False
  , isIll (zero :$ one) == True

  -- our Listable Expr enumeration does not produce ill typed Exprs
  , holds n $ isRight . etyp
  , holds n $ isJust  . mtyp
  , holds n $ not . isIll

  -- our Listable Ill enumeration only produces ill typed Exprs
  , holds n $ isLeft    . etyp . unIll
  , holds n $ isNothing . mtyp . unIll
  , holds n $ isIll . unIll

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
  , values (pp -&&- trueE) == [andE, pp, trueE]

  , holds n $ (okEqOrd :: Expr -> Expr -> Expr -> Bool)
  , holds n $ \(Ill e0) (Ill e1) (Ill e2) -> okEqOrd e0 e1 e2


  -- listing subexpressions

  , holds n $ \e -> isGround e ==> consts e == values e

  , holds n $ \e -> nubValues e `isSubsetOf` values e
  , holds n $ \e -> nubVars   e `isSubsetOf` vars   e
  , holds n $ \e -> nubConsts e `isSubsetOf` consts e
  , holds n $ \e -> vars      e `isSubsetOf` values e
  , holds n $ \e -> consts    e `isSubsetOf` values e
  , holds n $ \e -> (vars e ++ consts e) `isPermutationOf` values e
  , holds n $ \e -> (nubVars e ++ nubConsts e) `isPermutationOf` nubValues e
  ]
