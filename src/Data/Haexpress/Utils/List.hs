-- |
-- Module      : Data.Haexpress.Utils.List
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Re-exports the "Data.List" module along with additional functions over
-- lists.
module Data.Haexpress.Utils.List
  ( nubSort
  , isSubsetOf
  , module Data.List
  )
where

import Data.List

-- | /O(n log n)/.
-- Sorts and remove repetitions.
-- Equivalent to @nub . sort@.
--
-- > > nubSort [1,2,3]
-- > [1,2,3]
-- > > nubSort [3,2,1]
-- > [1,2,3]
-- > > nubSort [3,2,1,3,2,1]
-- > [1,2,3]
-- > > nubSort [3,3,1,1,2,2]
-- > [1,2,3]
nubSort :: Ord a => [a] -> [a]
nubSort  =  nnub . sort
  where
  -- linear nub of adjacent values
  nnub [] = []
  nnub [x] = [x]
  nnub (x:xs) = x : nnub (dropWhile (==x) xs)

-- | /O(n log n)/.
-- Checks that all elements of the first list are elements of the second.
isSubsetOf :: Ord a => [a] -> [a] -> Bool
xs `isSubsetOf` ys  =  nubSort xs `isSubsequenceOf` nubSort ys
  where
  -- only exported from Data.List since base 4.8.0.0
  isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
  isSubsequenceOf []    _                    =  True
  isSubsequenceOf (_:_) []                   =  False
  isSubsequenceOf (x:xs) (y:ys) | x == y     =     xs  `isSubsequenceOf` ys
                                | otherwise  =  (x:xs) `isSubsequenceOf` ys
