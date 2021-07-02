-- |
-- Module      : Data.Express.Utils.List
-- Copyright   : (c) 2019-2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Re-exports the "Data.List" module along with additional functions over
-- lists.
{-# LANGUAGE CPP #-}
module Data.Express.Utils.List
  ( nubSort
  , nubSortBy
  , isPermutationOf
  , isSubsetOf
  , isNub
  , lookupId
  , (+++)
  , module Data.List
#if __GLASGOW_HASKELL__ < 710
  , isSubsequenceOf
#endif
#ifdef __HUGS__
  , intercalate
#endif
  )
where

import Data.Function (on)
import Data.List
import Data.Maybe (fromMaybe)

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

-- | Like 'nubSort' but allows providing a function to 'compare' values.
nubSortBy :: (a -> a -> Ordering) -> [a] -> [a]
nubSortBy cmp  =  nnub . sortBy cmp
  where
  x -==- y  =  x `cmp` y == EQ
  -- linear nub of adjacent values
  nnub [] = []
  nnub [x] = [x]
  nnub (x:xs) = x : nnub (dropWhile (-==-x) xs)

-- | /O(n log n)/.
-- Checks that all elements of the first list are elements of the second.
isSubsetOf :: Ord a => [a] -> [a] -> Bool
xs `isSubsetOf` ys  =  nubSort xs `isSubsequenceOf` nubSort ys


#if __GLASGOW_HASKELL__ < 710
-- only exported from Data.List since base 4.8.0.0
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf []    _                    =  True
isSubsequenceOf (_:_) []                   =  False
isSubsequenceOf (x:xs) (y:ys) | x == y     =     xs  `isSubsequenceOf` ys
                              | otherwise  =  (x:xs) `isSubsequenceOf` ys
#endif

-- | /O(n log n)/.
-- Checks that all elements of the first list are elements of the second.
isPermutationOf :: Ord a => [a] -> [a] -> Bool
isPermutationOf  =  (==) `on` sort

-- | /O(n log n)/.
-- Checks that all elements are unique.
-- This function is a faster equivalent to the following:
--
-- > isNub xs  =  nub xs == xs
--
-- Examples:
--
-- > isNub []       =  True
-- > isNub [1,2,3]  =  True
-- > isNub [2,1,2]  =  False
isNub :: Ord a => [a] -> Bool
isNub xs  =  length (nubSort xs) == length xs

-- | /O(n)/.
-- Like 'lookup' but returns the key itself if nothing is found.
--
-- > > lookupId 5 [(1,2),(3,4)]
-- > 5
--
-- > > lookupId 5 [(1,2),(3,4),(5,6)]
-- > 6
lookupId :: Eq a => a -> [(a,a)] -> a
lookupId x = fromMaybe x . lookup x

-- | Merges two lists discarding repeated elements.
--
-- The argument lists need to be in order.
--
-- > > [1,10,100] +++ [9,10,11]
-- > [1,9,10,11,100]
(+++) :: Ord a => [a] -> [a] -> [a]
(+++)  =  nubMerge
infixr 5 +++

-- | Like 'nubMerge' but allows providing a function to 'compare' values.
nubMergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
nubMergeBy cmp (x:xs) (y:ys)  =  case x `cmp` y of
                                 LT -> x:nubMergeBy cmp xs (y:ys)
                                 GT -> y:nubMergeBy cmp (x:xs) ys
                                 EQ -> x:nubMergeBy cmp xs ys
nubMergeBy _ xs ys  =  xs ++ ys

-- | Merges two lists discarding repeated elements.
--
-- The argument lists need to be in order.
nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge  =  nubMergeBy compare

#ifdef __HUGS__
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss  =  concat (intersperse xs xss)
  where
  intersperse :: a -> [a] -> [a]
  intersperse _ []        =  []
  intersperse sep (x:xs)  =  x : prependToAll sep xs
    where
    prependToAll :: a -> [a] -> [a]
    prependToAll _   []      =  []
    prependToAll sep (x:xs)  =  sep : x : prependToAll sep xs
#endif
