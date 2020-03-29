{-# LANGUAGE TemplateHaskell, CPP #-}
-- |
-- Module      : Data.Express.Name.Derive
-- Copyright   : (c) 2019-2020 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Allows automatic derivation of 'Name' typeclass instances.
module Data.Express.Name.Derive
  ( deriveName
  , deriveNameCascading
  , deriveNameIfNeeded
  )
where

import qualified Data.Express.Name as N

import Control.Monad
import Data.Char
import Data.List
import Data.Express.Utils.TH

-- | Derives a 'N.Name' instance
--   for the given type 'Name'.
--
-- This function needs the @TemplateHaskell@ extension.
deriveName :: Name -> DecsQ
deriveName  =  deriveWhenNeededOrWarn ''N.Name reallyDeriveName

-- | Same as 'deriveName' but does not warn when instance already exists
--   ('deriveName' is preferable).
deriveNameIfNeeded :: Name -> DecsQ
deriveNameIfNeeded  =  deriveWhenNeeded ''N.Name reallyDeriveName

-- | Derives a 'N.Name' instance for a given type 'Name'
--   cascading derivation of type arguments as well.
deriveNameCascading :: Name -> DecsQ
deriveNameCascading  =  deriveWhenNeeded ''N.Name reallyDeriveNameCascading

reallyDeriveName :: Name -> DecsQ
reallyDeriveName t  =  do
  (nt,vs) <- normalizeType t
  [d| instance N.Name $(return nt) where
        name _ = $(stringE vname) |]
  where
  showJustName = reverse . takeWhile (/= '.') . reverse . show
  vname = map toLower . take 1 $ showJustName t
-- TODO: on deriveName, use full camelCase name?
-- TODO: on deriveName, use x for Num instances?

-- Not only really derive Name instances,
-- but cascade through argument types.
reallyDeriveNameCascading :: Name -> DecsQ
reallyDeriveNameCascading  =  reallyDeriveCascading ''N.Name reallyDeriveName
