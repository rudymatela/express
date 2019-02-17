-- Copyright (c) 2017-2018 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Haexpress.Utils.List

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True
  
  , evalInt (val (10 :: Int)) == 10
  , evalInt one == 1
  , holds n $ \x y -> evalInt (value "+" ((+) :: Int -> Int -> Int) :$ val x :$ val y) == x + y
  , holds n $ \x y -> evalInt (value "+" ((*) :: Int -> Int -> Int) :$ val x :$ val y) == x * y
  , holds n $ \i -> evalInt (val i) == i
  , holds n $ \e -> isJust (toDynamic e)
  , show (one -+- one) == "1 + 1 :: Int"
  , holds n $ \(IntE xx, IntE yy) -> isJust (toDynamic $ xx -+- yy)
  , holds n $ \(IntE xx, IntE yy) -> isGround xx && isGround yy
                                 ==> evalInt (xx -+- yy) == evalInt (yy -+- xx)

  -- tests to Name
  , name (undefined :: Int) == "x"
  , name (undefined :: Integer) == "x"
  , name (undefined :: Char) == "c"
  , name (undefined :: Bool) == "p"
  , name (undefined :: [Int]) == "xs"
  , name (undefined :: [[Int]]) == "xss"
  , name (undefined :: [Bool]) == "ps"
  , name (undefined :: [[Bool]]) == "pss"
  , name (undefined :: Either Bool Char) == "epc"
  , name (undefined :: Maybe Int) == "mx"
  , name (undefined :: Maybe [Int]) == "mxs"
  , name (undefined :: Maybe [[Int]]) == "mxss"

  , values (xx -+- yy) == [plusE, xx, yy]
  , values (xx -+- (yy -+- zz)) == [plusE, xx, plusE, yy, zz]
  , values ((xx -+- yy) -+- zz) == [plusE, plusE, xx, yy, zz]
  , values (zero -+- (one -*- two)) == [plusE, zero, timesE, one, two]
  , values (pp -&&- trueE) == [andE, pp, trueE]

  , holds n $ \e -> isGround e ==> consts e == values e

  , holds n $ \xs -> nubSort xs == nub (sort xs :: [Int])
  , holds n $ \xs -> nubSort xs == sort (nub xs :: [Int])

  , holds n $ isSubsetOf ==== (\xs ys -> all (`elem` ys) (xs :: [Int]))

  , holds n $ \e -> nubValues e `isSubsetOf` values e
  , holds n $ \e -> nubVars   e `isSubsetOf` vars   e
  , holds n $ \e -> nubConsts e `isSubsetOf` consts e
  , holds n $ \e -> vars      e `isSubsetOf` values e
  , holds n $ \e -> consts    e `isSubsetOf` values e
  , holds n $ \e -> (vars e ++ consts e) `isPermutationOf` values e
  , holds n $ \e -> (nubVars e ++ nubConsts e) `isPermutationOf` nubValues e
  ]
