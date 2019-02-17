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
  , module Data.List
  )
where

import Data.List

-- | Sorts and remove repetitions in @O (n * log n)@ time.
--   Equivalent to @nub . sort@.
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
