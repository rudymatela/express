-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

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
  , holds n $ \(IntE0 xx, IntE0 yy) -> evalInt (xx -+- yy) == evalInt (yy -+- xx)
  ]
