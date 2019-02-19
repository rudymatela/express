-- Copyright (c) 2019 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Haexpress.Utils.List (isNub)

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  -- TODO: tests that differentiate // and //-

  , ((xx -+- yy) -+- (yy -+- zz)) // [(yy,yy -+- zz)]
    == (xx -+- (yy -+- zz)) -+- ((yy -+- zz) -+- zz)

  , (xx -+- yy) // [(yy,yy -+- zz),(xx,xx -+- yy)]
    == (xx -+- yy) -+- (yy -+- zz)

  -- the order should not matter for //
  , holds n $ \e ee1 ee2 -> fst ee1 /= fst ee2 ==> e // [ee1, ee2] == e // [ee2, ee1]
  , holds n $ \e ees -> isNub (map fst ees) ==> e // ees == e // reverse ees

  -- the order should not matter for //-
  , holds n $ \e ee1 ee2 -> fst ee1 /= fst ee2 ==> e //- [ee1, ee2] == e //- [ee2, ee1]
  , holds n $ \e ees -> isNub (map fst ees) ==> e //- ees == e //- reverse ees

  -- equivalences between // and //-
  , holds n $ \e ees -> all (isVar . fst) ees ==> e // ees == e //- ees
  , holds n $ \e ees -> e // filter (isVar . fst) ees == e //- ees

  -- //- ignores replacements of non-variable values
  , holds n $ \e ees -> e //- filter (not . isVar . fst) ees == e
  ]
