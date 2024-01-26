-- Copyright (c) 2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \es e -> es >$$ e == es >$$< [e]
  , holds n $ \es e -> e $$< es == [e] >$$< es

  , [false, true, zero, one] >$$< [notE, andE, orE, plus, times] == []

  , [notE, andE, orE, plus, times] >$$< [false, true, zero, one]
    == [ not' false
       , not' true
       , andE :$ false
       , andE :$ true
       , orE :$ false
       , orE :$ true
       , plus :$ zero
       , plus :$ one
       , times :$ zero
       , times :$ one
       ]

  , [notE, andE, orE, plus, times] >$$< [false, true, zero, one] >$$< [false, true, zero, one]
    == [ false -&&- false
       , false -&&- true
       , true -&&- false
       , true -&&- true
       , false -||- false
       , false -||- true
       , true -||- false
       , true -||- true
       , zero -+- zero
       , zero -+- one
       , one -+- zero
       , one -+- one
       , zero -*- zero
       , zero -*- one
       , one -*- zero
       , one -*- one
       ]
  ]
