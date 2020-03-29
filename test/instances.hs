-- Copyright (c) 2017-2020 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE NoMonomorphismRestriction #-} -- ACK!
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , eval undefined (eqFor (undefined :: Int) :$ one :$ one) == True
  , eval undefined (eqFor (undefined :: Int) :$ one :$ two) == False

  , eval undefined (lessEqFor (undefined :: Int) :$ one :$ two) == True
  , eval undefined (lessEqFor (undefined :: Int) :$ one :$ one) == True
  , eval undefined (lessEqFor (undefined :: Int) :$ two :$ one) == False

  , eval undefined (lessFor (undefined :: Int) :$ one :$ two) == True
  , eval undefined (lessFor (undefined :: Int) :$ one :$ one) == False
  , eval undefined (lessFor (undefined :: Int) :$ two :$ one) == False

-- for the time being, compare has been removed from reifyOrd's result
--, eval undefined (compareFor (undefined :: Int) :$ one :$ two) == LT
--, eval undefined (compareFor (undefined :: Int) :$ one :$ one) == EQ
--, eval undefined (compareFor (undefined :: Int) :$ two :$ one) == GT

  , eval undefined (nameFor (undefined :: Int)  :$ xx) == "x"
  , eval undefined (nameFor (undefined :: Int)  :$ yy) == "x"
  , eval undefined (nameFor (undefined :: Bool) :$ pp) == "p"
  , eval undefined (nameFor (undefined :: Bool) :$ qq) == "p"

  , length (validApps functions one) == 5
  ]
  where
  eqFor = head . reifyEq
  lessEqFor = head . reifyOrd
  lessFor = head . tail . reifyOrd
--compareFor = head . reifyOrd
  nameFor = head . reifyName

functions :: [Expr]
functions  =  concat
  [ reifyEq (undefined :: Int)
  , reifyEq (undefined :: Bool)
  , reifyOrd (undefined :: Int)
  , reifyOrd (undefined :: Bool)
  , reifyName (undefined :: Int)
  , reifyName (undefined :: Bool)
  ]
