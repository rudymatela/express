-- Copyright (c) 2019 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \e1 e2 -> (e1,e2) == unpair (pair e1 e2)

  -- the result of pair always ill-typed
  , holds n $ \e1 e2 -> isIll $ pair e1 e2

  -- even though pair returns an ill-typed expression
  , holds n $ \e1 e2 -> pair e1 e2 == pair e1 e2
  , fails n $ \e1 e2 -> pair e1 e2 == pair e2 e1

  , show (pair xx yy) == "(x,y) :: ill-typed # ExprPair $ Int #"
  ]
