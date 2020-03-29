-- Copyright (c) 2019-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , isHole (hole (undefined :: Int))  == True
  , isHole (hole (undefined :: Bool)) == True
  , isHole (hole (undefined :: Char)) == True
  , holds n $ \x -> isHole (val (x :: Int))  == False
  , holds n $ \p -> isHole (val (p :: Bool)) == False
  , holds n $ \c -> isHole (val (c :: Char)) == False

  , holds n $ \e -> isHole e ==> isVar e
  , holds n $ \e1 e2 -> isHole (e1 :$ e2) == False

  , holds n $ \e -> holes e `isSubsequenceOf` vars e
  , holds n $ \e -> nubHoles e `isSubsetOf` holes e
  , holds n $ \e -> nubHoles e `isSubsequenceOf` nubVars e

  , [xx, yy, zz, xx'] `isPrefixOf` listVars "x" (undefined :: Int)
  , [pp, qq, rr, pp'] `isPrefixOf` listVars "p" (undefined :: Bool)
  , [xx, yy, zz, xx'] `isPrefixOf` listVarsAsTypeOf "x" zero
  , [pp, qq, rr, pp'] `isPrefixOf` listVarsAsTypeOf "p" false
  ]
