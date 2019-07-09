-- |
-- Module      : Data.Haexpress
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Expr' type and utilities involving it
{-# LANGUAGE CPP #-}
module Data.Haexpress
  ( 
-- TODO: explicitly export everything instead of the modules
    module Data.Haexpress.Basic
  , module Data.Haexpress.Canon
  , module Data.Haexpress.Match
  , module Data.Haexpress.Instances
  , module Data.Haexpress.Name
  , module Data.Haexpress.Express
  )
where

import Data.Haexpress.Basic
import Data.Haexpress.Canon
import Data.Haexpress.Match
import Data.Haexpress.Instances
import Data.Haexpress.Name
import Data.Haexpress.Express
