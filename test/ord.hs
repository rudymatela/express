-- Copyright (c) 2019 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ (okEqOrd :: Expr -> Expr -> Expr -> Bool)
  , holds n $ \(Ill e0) (Ill e1) (Ill e2) -> okEqOrd e0 e1 e2

  -- Holes < Values < Apps
  , xx < zero
  , zero < zero -+- one
  , xx < xx -+- yy
  , zero < xx -+- yy

  -- Less arity is less
  , zero < absE
  , absE < times
  , ae   < ordE
  , ordE < times
  , value "id" (id -:>  int)  < value "id"    (id    -:>  [int])
  , value "id" (id -:> [int]) < value "id"    (id    -:> [[int]])
  , value "id" (id -:>  int)  < value "sum"   (sum   -:>  [int])
  , value "id" (id -:>  int)  < value "(:[])" ((:[]) -:>   int)

  -- precedent types
  , pp < xx
  , cc < xx
  , pp < cc
  , xx < xxs
  , ae < zero
  , true < zero
  , true < ae
  , zero < nil
  ]
