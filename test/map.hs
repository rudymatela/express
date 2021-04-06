-- Copyright (c) 2019-2021 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import Test.LeanCheck.Function

import Data.Express.Utils.List (isNub)

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  -- the order should not matter for //
  , holds n $ \e ee1 ee2 -> fst ee1 /= fst ee2 ==> e // [ee1, ee2] == e // [ee2, ee1]
  , holds n $ \e ees -> isNub (map fst ees) ==> e // ees == e // reverse ees

  -- the order should not matter for //-
  , holds n $ \e ee1 ee2 -> fst ee1 /= fst ee2 ==> e //- [ee1, ee2] == e //- [ee2, ee1]
  , holds n $ \e ees -> isNub (map fst ees) ==> e //- ees == e //- reverse ees
  , holds n $ \e ees' -> let ees = map (mapFst unEV) ees' in
                         isNub (map fst ees) ==> e //- ees == e //- reverse ees

  -- //- and // are essentially different
  , exists n $ \e es ->  e // es  /=  e //- es

  -- equivalences between // and //-
  , holds n $ \e ees -> all (isValue . fst) ees ==> e // ees == e //- ees
  , holds n $ \e ees -> e // filter (isValue . fst) ees == e //- ees

  -- //- ignores replacements of non-variable values
  , holds n $ \e ees -> e //- filter (not . isValue . fst) ees == e

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

  -- tests of // and //-
  , ((xx -*- yy) -+- (yy -*- zz)) // [(xx,ii),(yy,jj),(zz,kk)] ==
    (ii -*- jj) -+- (jj -*- kk)

  , ((xx -*- yy) -+- (yy -*- zz)) //- [(xx,ii),(yy,jj),(zz,kk)] ==
    (ii -*- jj) -+- (jj -*- kk)

  , ((xx -*- yy) -+- (yy -*- zz)) // [(xx -*- yy,ii),(yy -*- zz,jj)] ==
    ii -+- jj

  , ((xx -*- yy) -+- (yy -*- zz)) //- [(xx -*- yy,ii),(yy -*- zz,jj)] ==
    (xx -*- yy) -+- (yy -*- zz)


  , ((xx -+- yy) -+- (yy -+- zz)) // [(yy,yy -+- zz)]
    == (xx -+- (yy -+- zz)) -+- ((yy -+- zz) -+- zz)

  , ((xx -+- yy) -+- (yy -+- zz)) //- [(yy,yy -+- zz)]
    == (xx -+- (yy -+- zz)) -+- ((yy -+- zz) -+- zz)

  , (xx -+- yy) // [(yy,yy -+- zz),(xx,xx -+- yy)]
    == (xx -+- yy) -+- (yy -+- zz)

  , (xx -+- yy) //- [(yy,yy -+- zz),(xx,xx -+- yy)]
    == (xx -+- yy) -+- (yy -+- zz)

  , ((xx -+- yy) -+- zz) // [(xx -+- yy, zero)] == (zero -+- zz)
  , (xx -+- (yy -+- zz)) // [(xx -+- yy, zero)] == (xx -+- (yy -+- zz))

  , holds n $ \(SameTypeE e1 e2)   ->          e1 // [(e1,e2)] == e2
  , holds n $ \(IntE e1) (IntE e2) -> (e1 -+- e1) // [(e1,e2)] == (e2 -+- e2)

  , holds n $ \e -> renameVarsBy id e == e
  , holds n $ \c e -> (renameVarsBy tail . renameVarsBy (c:)) e == e
  , renameVarsBy (++ "1") (xx -+- yy) == (var "x1" int -+- var "y1" int)
  , renameVarsBy (\(c:cs) -> succ c:cs) ((xx -+- yy) -+- ord' cc)
                                 == ((yy -+- zz) -+- ord' dd)
  ]

mapFst :: (a -> c) -> (a,b) -> (c,b)
mapFst f (x,y) = (f x, y)
