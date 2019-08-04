-- |
-- Module      : Data.Express.Basic
-- Copyright   : (c) 2019 Rudy Matela
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
  )
where

import Data.Express.Core
import Data.Express.Map
import Data.Express.Fold
import Data.Express.Hole
