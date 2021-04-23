-- Copyright (c) 2019-2021 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ (okEqOrd :: Expr -> Expr -> Expr -> Bool)
  , holds n $ \(Ill e0) (Ill e1) (Ill e2) -> okEqOrd e0 e1 e2
  , holds n $ compare ==== compareComplexity <> compareLexicographically

  , holds n $ isComparison (compare :: Expr -> Expr -> Ordering)
  , holds n $ isComparison compareLexicographically
  , holds n $ isComparison compareQuickly

  , exists n $ \e1 e2 ->        e1 `compare` e2 /= e1 `compareLexicographically` e2
  , exists n $ \e1 e2 ->        e1 `compare` e2 /= e1 `compareQuickly` e2
  , exists n $ \e1 e2 -> e1 `compareQuickly` e2 /= e1 `compareLexicographically` e2

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

  -- other precedence rules
  ,  (xx -+- xx)         < (xx -+- (xx -+- xx))
  , ((xx -+- xx) -+- xx) > (xx -+- (xx -+- xx))
  , xx < yy
  , zero < one
  , xx < zero
  ]
