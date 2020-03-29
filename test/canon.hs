-- Copyright (c) 2017-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , canonicalize (xx -+- yy)
              == (xx -+- yy)
  , canonicalize (jj -+- (ii -+- ii))
              == (xx -+- (yy -+- yy))
  , canonicalize ((jj -+- ii) -+- (xx -+- xx))
              == ((xx -+- yy) -+- (zz -+- zz))

  -- these are just tests:
  -- canonicalizeWith expects the resulting list of the arg function to be infinite
  , canonicalizeWith (const ["i","j","k","l"]) (xx -+- yy)
                                            == (ii -+- jj)
  , canonicalizeWith (const ["i","j","k","l"]) (jj -+- (ii -+- ii))
                                            == (ii -+- (jj -+- jj))
  , canonicalizeWith (const ["i","j","k","l"]) ((jj -+- ii) -+- (xx -+- xx))
                                            == ((ii -+- jj) -+- (kk -+- kk))

  , canonicalize (xx -+- ord' cc) == (xx -+- ord' cc)
  , canonicalize (yy -+- ord' dd) == (xx -+- ord' cc)
  , canonicalize (ff xx -+- gg yy -+- ff yy) == (ff xx -+- gg yy -+- ff yy)
  , canonicalize (gg yy -+- ff xx -+- gg xx) == (ff xx -+- gg yy -+- ff yy)
  , canonicalizeWith (lookupNames $ reifyName (undefined :: Int -> Int -> Int) ++ preludeNameInstances)
      (zz -?- kk) == (var "f" (undefined :: Int -> Int -> Int) :$ xx :$ yy)

  -- canonicalizing holes --
  , canonicalize (hole (undefined :: Int       )) == xx
  , canonicalize (hole (undefined :: Bool      )) == pp
  , canonicalize (hole (undefined :: Char      )) == cc
  , canonicalize (hole (undefined :: [Int]     )) == xxs
  , canonicalize (hole (undefined :: [Char]    )) == ccs
  , canonicalize (hole (undefined :: ()        )) == var "u" ()
  , canonicalize (hole (undefined :: Integer   )) == var "x" (undefined :: Integer)
  , canonicalize (hole (undefined :: [Integer] )) == var "xs" (undefined :: [Integer])
  , canonicalize (hole (undefined :: Maybe Int )) == var "mx" (undefined :: Maybe Int)
  , canonicalize (hole (undefined :: (Int,Int) )) == var "xy" (undefined :: (Int,Int))

  , canonicalVariations (zero -+- xx) == [zero -+- xx]
  , canonicalVariations (zero -+- i_) == [zero -+- xx]
  , canonicalVariations (i_ -+- i_) == [xx -+- yy, xx -+- xx]
  , canonicalVariations (i_ -+- (i_ -+- ord' c_))
    == [ xx -+- (yy -+- ord' cc)
       , xx -+- (xx -+- ord' cc) ]

  , fastCanonicalVariations (ii -+- i_) == [ii -+- xx]
  , canonicalVariations ((i_ -+- i_) -+- (ord' c_ -+- ord' c_))
    == [ (xx -+- yy) -+- (ord' cc -+- ord' dd)
       , (xx -+- yy) -+- (ord' cc -+- ord' cc)
       , (xx -+- xx) -+- (ord' cc -+- ord' dd)
       , (xx -+- xx) -+- (ord' cc -+- ord' cc) ]

  , canonicalVariations (i_)
    == [ xx ]
  , canonicalVariations (i_ -+- i_)
    == [ xx -+- yy
       , xx -+- xx ]
  , canonicalVariations (i_ -+- i_ -+- i_)
    == [ xx -+- yy -+- zz
       , xx -+- yy -+- xx
       , xx -+- yy -+- yy
       , xx -+- xx -+- yy
       , xx -+- xx -+- xx
       ]
  , canonicalVariations (i_ -+- i_ -+- i_ -+- i_)
    == [ xx -+- yy -+- zz -+- xx'
       , xx -+- yy -+- zz -+- xx
       , xx -+- yy -+- zz -+- yy
       , xx -+- yy -+- zz -+- zz
       , xx -+- yy -+- xx -+- zz
       , xx -+- yy -+- xx -+- xx
       , xx -+- yy -+- xx -+- yy
       , xx -+- yy -+- yy -+- zz
       , xx -+- yy -+- yy -+- xx
       , xx -+- yy -+- yy -+- yy
       , xx -+- xx -+- yy -+- zz
       , xx -+- xx -+- yy -+- xx
       , xx -+- xx -+- yy -+- yy
       , xx -+- xx -+- xx -+- yy
       , xx -+- xx -+- xx -+- xx
       ]

  , holds n $ \e -> all isHole (vars e)
                ==> let xs = map (length . nubVars) $ canonicalVariations e
                    in (head xs >) `all` tail xs
                    && (last xs <) `all` init xs
  , holds n $ \e -> all isHole (vars e)
                ==> isNub (vars (head (canonicalVariations e)))
  , holds n $ \e -> all isHole (vars e)
                ==> let es = canonicalVariations e
                    in (`isInstanceOf` head es) `all` tail es
                    && (last es `isInstanceOf`) `all` init es
  , holds n $ \e -> let es = canonicalVariations e
                    in length (nub (sort es)) == length es
  , holds n $ \e -> length (canonicalVariations e)
                 == product (map (bell . snd) . counts $ holes e)
  ]

-- O(1) bell number implementation
-- only works up to 8
-- but this is enough for testing.
bell :: Int -> Int
bell 0 = 1
bell 1 = 1
bell 2 = 2
bell 3 = 5
bell 4 = 15
bell 5 = 52
bell 6 = 203
bell 7 = 877
bell 8 = 4140
bell _ = error "bell: argument > 8, implement me!"
