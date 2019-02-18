-- Copyright (c) 2019 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
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
  , values (xx -+- yy) == [plusE, xx, yy]

  , ((xx -+- yy) -+- (yy -+- zz)) // [("y",yy -+- zz)]
    == (xx -+- (yy -+- zz)) -+- ((yy -+- zz) -+- zz)

  , (xx -+- yy) // [("y",yy -+- zz),("x",xx -+- yy)]
    == (xx -+- yy) -+- (yy -+- zz)
  ]
