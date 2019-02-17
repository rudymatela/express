-- Copyright (c) 2017-2018 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Haexpress.Utils.List

main :: IO ()
main = mainTest tests 360

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \xs -> nubSort xs == nub (sort xs :: [Int])
  , holds n $ \xs -> nubSort xs == sort (nub xs :: [Int])

  , holds n $ isSubsetOf ==== (\xs ys -> all (`elem` ys) (xs :: [Int]))
  ]
