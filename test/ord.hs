-- Copyright (c) 2019 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ (okEqOrd :: Expr -> Expr -> Expr -> Bool)
  , holds n $ \(Ill e0) (Ill e1) (Ill e2) -> okEqOrd e0 e1 e2
  ]
