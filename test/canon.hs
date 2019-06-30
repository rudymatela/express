-- Copyright (c) 2017-2018 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
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

  , canonicalize (hole (undefined :: Int       )) == xx
  , canonicalize (hole (undefined :: Bool      )) == pp
  , canonicalize (hole (undefined :: Char      )) == cc
  , canonicalize (hole (undefined :: [Int]     )) == xxs
  , canonicalize (hole (undefined :: [Char]    )) == ccs
  , canonicalize (hole (undefined :: ()        )) == var "u" ()
  , canonicalize (hole (undefined :: Integer   )) == var "x" (undefined :: Integer)
  , canonicalize (hole (undefined :: [Integer] )) == var "xs" (undefined :: [Integer])
  , canonicalize (hole (undefined :: Maybe Int )) == var "mx" (undefined :: Maybe Int)
-- TODO: make me pass
--, canonicalize (hole (undefined :: (Int,Int) )) == var "xy" (undefined :: (Int,Int))
  ]
