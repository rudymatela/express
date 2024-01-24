-- Copyright (c) 2019-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Express.Utils.List

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , evl (val (10 :: Int)) == (10 :: Int)
  , evl one == (1 :: Int)
  , holds n $ \x y -> evl (value "+" ((+) :: Int -> Int -> Int) :$ val x :$ val y) == (x + y :: Int)
  , values (xx -+- yy) == [plus, xx, yy]
  , (xx -+- yy) // [(yy,yy -+- zz),(xx,xx -+- yy)] == (xx -+- yy) -+- (yy -+- zz)
  ]
