-- |
-- Module      : Data.Express.Utils.List
-- Copyright   : (c) 2019-2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Re-exports a few standard Haskell modules module along with additional
-- functions.
{-# LANGUAGE CPP #-}
module Data.Express.Utils
  ( module Data.Express.Utils.List
  , module Data.Express.Utils.String
  , module Data.Monoid
  , module Data.Maybe
  , module Data.Either
  , module Data.Function
#if __GLASGOW_HASKELL__ < 704
  , (<>)
#endif
  )
where

import Data.Express.Utils.List
import Data.Express.Utils.String
import Data.Function
import Data.Maybe
import Data.Either
import Data.Monoid

#if __GLASGOW_HASKELL__ < 704
-- Data.Monoid exports <> since GHC 7.4 / base 4.5.0.0
-- GHC 7.2 / base 4.4.1.0 / Hugs 2006.9 do not define <>
(<>) :: Monoid m => m -> m -> m
(<>)  =  mappend
infixr 6 <>
#endif
