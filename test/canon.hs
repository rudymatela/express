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

-- TODO: make me pass
--, canonicalize (xx -+- ord' cc) == (xx -+- ord' cc)
  ]
