-- Copyright (c) 2019-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main  =  mainTest tests 5040

tests :: Int -> [Bool]
tests n  =
  [ True

  , isHole (hole (undefined :: Int))  == True
  , isHole (hole (undefined :: Bool)) == True
  , isHole (hole (undefined :: Char)) == True
  , holds n $ \x -> isHole (val (x :: Int))  == False
  , holds n $ \p -> isHole (val (p :: Bool)) == False
  , holds n $ \c -> isHole (val (c :: Char)) == False

  , holds n $ \e -> isHole e ==> isVar e
  , holds n $ \e1 e2 -> isHole (e1 :$ e2) == False
  , holds n $ \e -> isHole e ==> hasHole e
  , holds n $ \e1 e2 -> (hasHole e1 || hasHole e2) == hasHole (e1 :$ e2)
  , holds n $ \e -> hasHole e == (not . null . holes) e
  , holds n $ \e -> isComplete e == not (hasHole e)

  , holds n $ \e -> holes e `isSubsequenceOf` vars e
  , holds n $ \e -> nubHoles e `isSubsetOf` holes e
  , holds n $ \e -> nubHoles e `isSubsequenceOf` nubVars e

  , [xx, yy, zz, xx'] `isPrefixOf` listVars "x" (undefined :: Int)
  , [pp, qq, rr, pp'] `isPrefixOf` listVars "p" (undefined :: Bool)
  , [xx, yy, zz, xx'] `isPrefixOf` listVarsAsTypeOf "x" zero
  , [pp, qq, rr, pp'] `isPrefixOf` listVarsAsTypeOf "p" false

  -- fill unit tests
  , fill (i_ -+- i_) [xx, yy] == xx -+- yy
  , fill (i_ -+- i_) [xx, xx] == xx -+- xx
  , fill (i_ -+- i_) [one, one -+- one] == one -+- (one -+- one)

  -- silent behaviours of fill
  , fill (i_ -+- i_ -+- i_) [xx, yy] == xx -+- yy -+- i_
  , fill (i_) [xx, yy] == xx
  , fill (i_ -+- i_ -+- i_) [xx, val 'c', yy] == xx -+- i_ -+- i_

  -- fill properties
  , holds n $ \(IntE e) -> fill (zero -+- i_ -+- two) [e] == zero -+- e -+- two
  , holds n $ \(IntE e1, IntE e2) -> fill (i_ -+- one -+- i_) [e1, e2] == e1 -+- one -+- e2
  ]
