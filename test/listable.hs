-- Copyright (c) 2017-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  -- Listable Expr only produces well-typed expressions
  , holds n $ isJust . toDynamic
  , holds n $ isJust . mtyp

  -- Listable Ill only produces ill-typed expressions
  , holds n $ isNothing . toDynamic . unIll
  , holds n $ isNothing . mtyp      . unIll

  -- Listable TypeE produces expressions of the right type (evaluation)
  , holds n $ isJust . evaluateInt      . unIntE
  , holds n $ isJust . evaluateBool     . unBoolE
  , holds n $ isJust . evaluateInts     . unIntsE
  , holds n $ isJust . evaluateIntToInt . unIntToIntE
  , holds n $ isJust . evaluateChar     . unCharE
  , holds n $ \(IntToIntE ff) (IntE xx) -> isJust . evaluateInt $ ff :$ xx
  , holds n $ \(IntToIntToIntE ff) (IntE xx) (IntE yy) -> isJust . evaluateInt $ ff :$ xx :$ yy

  -- Listable TypeE produces expressions of the right type (typ)
  , holds n $ \(SameTypeE e1 e2) -> typ e1 == typ e2
  , holds n $ \(SameTypedPairsE ees) -> all (\(e1,e2) -> typ e1 == typ e2) ees
  , holds n $ \(IntE  e) -> typ e == typ i_
  , holds n $ \(BoolE e) -> typ e == typ b_
  , holds n $ \(CharE e) -> typ e == typ c_
  , holds n $ \(IntsE e) -> typ e == typ is_

  -- Listable TypeE does not produce expressions of the wrong type
  , holds n $ isNothing . evaluateInt      . unBoolE
  , holds n $ isNothing . evaluateBool     . unIntE
  , holds n $ isNothing . evaluateInts     . unIntE
  , holds n $ isNothing . evaluateIntToInt . unIntE
  , holds n $ isNothing . evaluateChar     . unIntE

  -- Listable TypeE0 only returns terminal constants
  , holds n $ isConst . unE0
  , holds n $ isConst . unIntE0
  , holds n $ isConst . unBoolE0
  , holds n $ isConst . unIntsE0
  , holds n $ isConst . unCharE0

  -- Listable TypeEV only returns variables
  , holds n $ isVar . unEV
  , holds n $ isVar . unIntEV
  , holds n $ isVar . unBoolEV
  , holds n $ isVar . unIntsEV
  , holds n $ isVar . unCharEV

  -- counter-examples are of the right type
  , counterExample n (\(IntE xx) -> False) == Just ["_ :: Int"]

  , isNub (take (n`div`10) list :: [Expr])
  , isNub (take (n`div`10) $ map unSameTypeE list)
  , isNub (take (n`div`10) $ map unIntE list)
  ]

evaluateInt :: Expr -> Maybe Int
evaluateInt  =  evaluate

evaluateBool :: Expr -> Maybe Bool
evaluateBool  =  evaluate

evaluateInts :: Expr -> Maybe [Int]
evaluateInts  =  evaluate

evaluateIntToInt :: Expr -> Maybe (Int -> Int)
evaluateIntToInt  =  evaluate

evaluateChar :: Expr -> Maybe Char
evaluateChar  =  evaluate
