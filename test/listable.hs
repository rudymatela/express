-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \(IntE xx) -> isJust $ evaluateInt xx
  , holds n $ \(FunE_II ff) (IntE xx) -> isJust . evaluateInt $ ff :$ xx
  , holds n $ \(FunE_III ff) (IntE xx) (IntE yy) -> isJust . evaluateInt $ ff :$ xx :$ yy
  , holds n $ \(FunE ff) -> isNothing $ evaluateInt ff
  , holds n $ \(IntE xx) -> isNothing $ evaluateIntToInt xx

  , (counterExample n $ \(IntE xx) -> False) == Just ["_ :: Int"]
  ]

evaluateIntToInt :: Expr -> Maybe (Int -> Int)
evaluateIntToInt = evaluate
