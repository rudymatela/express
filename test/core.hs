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

  , holds n $ \(FunE_II ff, IntE xx)  -> isJust (ff $$ xx)
--, holds n $ \(FunE_II ff, BoolE pp) -> isNothing (ff $$ pp) -- TODO


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