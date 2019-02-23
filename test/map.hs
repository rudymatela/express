-- Copyright (c) 2019 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import Test.LeanCheck.Function

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
  , holds n $ \e ees' -> let ees = map (mapFst unEV) ees' in
                         isNub (map fst ees) ==> e //- ees == e //- reverse ees

  -- equivalences between // and //-
  , holds n $ \e ees -> all (isVar . fst) ees ==> e // ees == e //- ees
  , holds n $ \e ees -> e // filter (isVar . fst) ees == e //- ees

  -- //- ignores replacements of non-variable values
  , holds n $ \e ees -> e //- filter (not . isVar . fst) ees == e

  -- (in)equivalences between maps
  , exists n $ \f e -> mapValues f e /= (mapVars f . mapConsts f) e
  , exists n $ \f e -> mapValues f e /= (mapConsts f . mapVars f) e
  , exists n $ \f e -> (mapVars f . mapConsts f) e /= (mapConsts f . mapVars f) e
  , exists n $ \f e -> (mapConsts f . mapVars f) e /= (mapVars f . mapConsts f) e
  -- the above should fail because of the following
  , let f _ = id' i_ in mapValues f zero == id' i_
                     && mapVars   f zero == zero
                     && mapConsts f zero == id' i_
                     && mapVars f (mapConsts f zero) == id' (id' i_)
                     && mapConsts f (mapVars f zero) == id' i_

  -- the following do not hold in general:
  , exists n $ \f e -> (mapValues f . mapValues f) e /= mapValues f e
  , exists n $ \f e -> (mapVars   f . mapVars   f) e /= mapVars   f e
  , exists n $ \f e -> (mapConsts f . mapConsts f) e /= mapConsts f e

  -- what actually holds is this:
  , holds n $ \f e -> (mapValues f . mapValues f) e == mapValues (mapValues f . f) e
  , holds n $ \f e -> (mapVars   f . mapVars   f) e == mapVars   (mapVars   f . f) e
  , holds n $ \f e -> (mapConsts f . mapConsts f) e == mapConsts (mapConsts f . f) e

  -- the following do not hold in general:
  , exists n $ \f e -> values (mapValues f e) /= map f (values e)
  , exists n $ \f e -> vars   (mapVars   f e) /= map f (vars   e)
  , exists n $ \f e -> consts (mapConsts f e) /= map f (consts e)

  -- what actually holds is this:
  , holds n $ \f e -> values (mapValues f e) == concatMap (values . f) (values e)
  , holds n $ \f e -> vars   (mapVars   f e) == concatMap (vars   . f) (vars   e)
  , holds n $ \f e -> consts (mapConsts f e) == concatMap (consts . f) (consts e)
  ]

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (x,y) = (f x, y)
