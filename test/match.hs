-- Copyright (c) 2019 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \(IntE e)            -> e `isInstanceOf` xx
  , holds n $ \(IntE e)            -> abs' e `isInstanceOf` abs' xx
  , holds n $ \(IntE e)            -> (e -+- e) `isInstanceOf` (xx -+- xx)
  , holds n $ \(IntE e1) (IntE e2) -> (e1 -+- e2) `isInstanceOf` (xx -+- yy)
  , holds n $ \(IntE e1) (IntE e2) -> e1 /= e2 ==> not ((e1 -+- e2) `isInstanceOf` (xx -+- xx))
  , holds n $ \e                   -> e /= zero ==> not (e `isInstanceOf` zero)

  ,       (zero -+- one)       `isInstanceOf` (xx -+- yy)
  ,       (zero -+- zero)      `isInstanceOf` (xx -+- yy)
  ,       (yy -+- xx)          `isInstanceOf` (xx -+- yy)
  ,       (zero -+- zero)      `isInstanceOf` (xx -+- xx)
  , not $ (zero -+- one)       `isInstanceOf` (xx -+- xx)
  ,       zero                 `isInstanceOf`          xx
  , not $ xx                   `isInstanceOf`        zero
  ,       (xx -+- (yy -+- xx)) `isInstanceOf` (xx -+- yy)
  ,       (xx -+- (xx -+- xx)) `isInstanceOf` (xx -+- yy)
  , not $ (xx -+- (xx -+- xx)) `isInstanceOf` (xx -+- xx)

  , holds n $ \(IntE e1) (IntE e2) -> match (e1 -+- e2) (xx -+- yy) == Just [(yy,e2),(xx,e1)]
  , holds n $ \(IntE e)            -> match (e -+- e)   (xx -+- xx) == Just [(xx,e)]
  , holds n $ \(IntE e1) (IntE e2) -> e1 /= e2 ==> match (e1 -+- e2) (xx -+- xx) == Nothing
  , holds n $ \(IntE e1) (IntE e2) (IntE e3) -> e2 /= e3
                ==> match ((e1 -+- e1) -+- (e2 -+- e3)) (xx -+- (yy -+- yy)) == Nothing
  , holds n $ \(IntE e1) (IntE e2) -> matchWith [(xx,e1)] (e1 -+- e2) (xx -+- yy) == Just [(yy,e2),(xx,e1)]
  , holds n $ \(IntE e1) (IntE e2) -> e1 /= e2 ==> matchWith [(xx,e2)] (e1 -+- e2) (xx -+- yy) == Nothing
  , holds n $ \e1 e2 -> e1 `match` e2 == matchWith [] e1 e2
  , holds n $ \(SameTypeE e1 e2) (SameTypeE e3 e4) ->
                not (isFunTy $ typ e1) && not (isFunTy $ typ e3)
                  ==>
                (e1 -==- e2) `match` (e3 -==- e4) == foldPair (e1,e2) `match` foldPair (e3,e4)
  ]