-- Copyright (c) 2019 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \x -> expr x == val (x :: Int)
  , holds n $ \c -> expr c == val (c :: Char)
  , holds n $ \p -> expr p == val (p :: Bool)
  , holds n $ \x -> expr x == val (x :: ())
  , fails n $ \xs -> expr xs == val (xs :: [Int])
  , holds n $ expr ([] :: [Int]) == val ([] :: [Int])
  , holds n $ \x xs -> expr (x:xs) /= val (x:xs :: [Int])
  ]

evaluateIntToInt :: Expr -> Maybe (Int -> Int)
evaluateIntToInt = evaluate
