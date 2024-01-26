-- |
-- Module      : Data.Express.Basic
-- Copyright   : (c) 2019-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Expr' type and _basic_ utilities involving it, including:
--
-- * re-export of "Data.Express.Core"
-- * re-export of "Data.Express.Map"
-- * re-export of "Data.Express.Fold"
-- * re-export of "Data.Express.Hole"
--
-- If you're a Express user,
-- you're probably better of importing "Data.Express".
{-# LANGUAGE CPP #-}
module Data.Express.Basic
  (
  -- * Module re-exports
    module Data.Express.Core
  , module Data.Express.Map
  , module Data.Express.Fold
  , module Data.Express.Hole

  -- * Additional utilities
  , (>$$<)
  , (>$$)
  , ($$<)
  )
where

import Data.Express.Core
import Data.Express.Map
import Data.Express.Fold
import Data.Express.Hole

import Data.Maybe (catMaybes, mapMaybe)

-- | Lists valid applications between lists of 'Expr's
--
-- > > [notE, plus] >$$< [false, true, zero]
-- > [not False :: Bool,not True :: Bool,(0 +) :: Int -> Int]
(>$$<) :: [Expr] -> [Expr] -> [Expr]
efs >$$< exs  =  catMaybes [ef $$ ex | ef <- efs, ex <- exs]

-- | Lists valid applications between a list of 'Expr's and an 'Expr'.
--
-- > > [plus, times] >$$ zero
-- > [(0 +) :: Int -> Int,(0 *) :: Int -> Int]
(>$$) :: [Expr] -> Expr -> [Expr]
efs >$$ ex  =  mapMaybe ($$ ex) efs

-- | Lists valid applications between an 'Expr' and a list of 'Expr's.
--
-- > > notE >$$< [false, true, zero]
-- > [not False :: Bool,not True :: Bool]
($$<) :: Expr -> [Expr] -> [Expr]
ef $$< exs  =  mapMaybe (ef $$) exs
