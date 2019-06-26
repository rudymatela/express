-- Copyright (c) 2017-2018 Rudy Matela.  -- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Data.Haexpress.Utils.List
import Data.Haexpress.Utils.Typeable

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \xs -> nubSort xs == nub (sort xs :: [Int])
  , holds n $ \xs -> nubSort xs == sort (nub xs :: [Int])

  , holds n $ isSubsetOf ==== (\xs ys -> all (`elem` ys) (xs :: [Int]))

  , elementTy (typeOf [True]) == boolTy
  , elementTy (elementTy (typeOf [[False]])) == boolTy
  ]
