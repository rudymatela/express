-- Copyright (c) 2019 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True

  , ((xx -+- yy) -+- (yy -+- zz)) // [(yy,yy -+- zz)]
    == (xx -+- (yy -+- zz)) -+- ((yy -+- zz) -+- zz)

  , (xx -+- yy) // [(yy,yy -+- zz),(xx,xx -+- yy)]
    == (xx -+- yy) -+- (yy -+- zz)
  ]
