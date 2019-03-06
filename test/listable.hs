-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \e -> isJust (toDynamic e)

  , holds n $ \(IntE xx) -> isJust $ evaluateInt xx
  , holds n $ \(BoolE pp) -> isJust $ evaluateBool pp
  , holds n $ \(IntsE xxs) -> isJust $ evaluateInts xxs
  , holds n $ \(IntToIntE ff) -> isJust $ evaluateIntToInt ff

  , holds n $ \(IntToIntE ff) (IntE xx) -> isJust . evaluateInt $ ff :$ xx
  , holds n $ \(IntToIntToIntE ff) (IntE xx) (IntE yy) -> isJust . evaluateInt $ ff :$ xx :$ yy
  , holds n $ \(IntE xx) -> isNothing $ evaluateIntToInt xx
  , holds n $ \(IntE xx) -> isNothing $ evaluateBool xx
  , holds n $ \(BoolE pp) -> isNothing $ evaluateInt pp

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
