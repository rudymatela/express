-- Copyright (c) 2017-2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True

  , eval undefined (eqFor (undefined :: Int) :$ one :$ one) == True
  , eval undefined (eqFor (undefined :: Int) :$ one :$ two) == False

  , eval undefined (compareFor (undefined :: Int) :$ one :$ two) == LT
  , eval undefined (compareFor (undefined :: Int) :$ one :$ one) == EQ
  , eval undefined (compareFor (undefined :: Int) :$ two :$ one) == GT

  , eval undefined (nameFor (undefined :: Int)  :$ xx) == "x"
  , eval undefined (nameFor (undefined :: Int)  :$ yy) == "x"
  , eval undefined (nameFor (undefined :: Bool) :$ pp) == "p"
  , eval undefined (nameFor (undefined :: Bool) :$ qq) == "p"

  , length (validApps functions one) == 3
  ]

functions :: [Expr]
functions  =  [ eqFor (undefined :: Int)
              , eqFor (undefined :: Bool)
              , compareFor (undefined :: Int)
              , compareFor (undefined :: Bool)
              , nameFor (undefined :: Int)
              , nameFor (undefined :: Bool)
              ]
