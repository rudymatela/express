-- |
-- Module      : Data.Haexpress.Basic
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Expr' type and _basic_ utilities involving it, including:
--
-- * re-export of "Data.Haexpress.Core"
-- * re-export of "Data.Haexpress.Map"
-- * re-export of "Data.Haexpress.Fold"
-- * re-export of "Data.Haexpress.Hole"
--
-- If you're a Haexpress user,
-- you're probably better of importing "Data.Haexpress".
{-# LANGUAGE CPP #-}
module Data.Haexpress.Basic
  (
  -- * Module re-exports
    module Data.Haexpress.Core
  , module Data.Haexpress.Map
  , module Data.Haexpress.Fold
  , module Data.Haexpress.Hole
  )
where

import Data.Haexpress.Core
import Data.Haexpress.Map
import Data.Haexpress.Fold
import Data.Haexpress.Hole
