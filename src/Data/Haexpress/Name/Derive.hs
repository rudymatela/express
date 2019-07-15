{-# LANGUAGE TemplateHaskell, CPP #-}
-- |
-- Module      : Data.Haexpress.Name.Derive
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Allows automatic derivation of 'Name' typeclass instances.
module Data.Haexpress.Name.Derive
  ( deriveName
  , deriveNameCascading
  , deriveNameIfNeeded
  )
where

import qualified Data.Haexpress.Name as N

import Control.Monad
import Data.Char
import Data.List
import Data.Haexpress.Utils.TH

-- This function needs the @TemplateHaskell@ extension.
deriveName :: Name -> DecsQ
deriveName = deriveNameX True False

-- | Same as 'deriveName' but does not warn when instance already exists
--   ('deriveName' is preferable).
deriveNameIfNeeded :: Name -> DecsQ
deriveNameIfNeeded = deriveNameX False False

-- | Derives a 'Name' instance for a given type 'Name'
--   cascading derivation of type arguments as well.
deriveNameCascading :: Name -> DecsQ
deriveNameCascading = deriveNameX True True

deriveNameX :: Bool -> Bool -> Name -> DecsQ
deriveNameX warnExisting cascade t  =  do
  is <- t `isInstanceOf` ''N.Name
  if is
  then do
    unless (not warnExisting)
      (reportWarning $ "Instance Name " ++ show t
                    ++ " already exists, skipping derivation")
    return []
  else
    if cascade
    then reallyDeriveNameCascading t
    else reallyDeriveName t

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
reallyDeriveNameCascading t =
      return . concat
  =<< mapM reallyDeriveName
  =<< filterM (liftM not . isTypeSynonym)
  =<< return . (t:) . delete t
  =<< t `typeConCascadingArgsThat` (`isntInstanceOf` ''N.Name)
