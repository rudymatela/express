-- Copyright (c) 2019-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \e1 e2 -> (e1,e2) == unfoldPair (foldPair (e1,e2))
  , holds n $ \e123 -> e123 == unfoldTrio (foldTrio e123)

  -- the result of foldPair and foldTrio is always ill-typed
  , holds n $ \e1 e2 -> isIllTyped $ foldPair (e1,e2)
  , holds n $ \e123 -> isIllTyped $ foldTrio e123

  -- (==) works even though foldPair returns an ill-typed expression
  , holds n $ \e1 e2 -> foldPair (e1,e2) == foldPair (e1,e2)
  , fails n $ \e1 e2 -> foldPair (e1,e2) == foldPair (e2,e1)

  , show (foldPair (xx,yy)) == "(x,y) :: ill-typed # ExprPair $ Int #"
  , show (foldTrio (xx,yy,zz)) == "(x,y,z) :: ill-typed # ExprTrio $ Int #"

  , unfoldApp (abs' xx)          == [absE, xx]
  , unfoldApp (abs' (xx -+- yy)) == [absE, xx -+- yy]
  , unfoldApp (xx -+- abs' xx)   == [plus, xx, abs' xx]
  , unfoldApp one                == [one]
  , unfoldApp false              == [false]

  , holds n $ \e -> foldApp (unfoldApp e) == e

  , holds n $ \es -> es == unfold (fold es)
  ]
