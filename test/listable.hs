-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ isJust . toDynamic

  , holds n $ isJust . evaluateInt      . unIntE
  , holds n $ isJust . evaluateBool     . unBoolE
  , holds n $ isJust . evaluateInts     . unIntsE
  , holds n $ isJust . evaluateIntToInt . unIntToIntE

  , holds n $ \(IntToIntE ff) (IntE xx) -> isJust . evaluateInt $ ff :$ xx
  , holds n $ \(IntToIntToIntE ff) (IntE xx) (IntE yy) -> isJust . evaluateInt $ ff :$ xx :$ yy

  , holds n $ isNothing . evaluateInt      . unBoolE
  , holds n $ isNothing . evaluateBool     . unIntE
  , holds n $ isNothing . evaluateInts     . unIntE
  , holds n $ isNothing . evaluateIntToInt . unIntE

  , (counterExample n $ \(IntE xx) -> False) == Just ["_ :: Int"]
  ]

evaluateInt :: Expr -> Maybe Int
evaluateInt = evaluate

evaluateBool :: Expr -> Maybe Bool
evaluateBool = evaluate

evaluateInts :: Expr -> Maybe [Int]
evaluateInts = evaluate

evaluateIntToInt :: Expr -> Maybe (Int -> Int)
evaluateIntToInt = evaluate
