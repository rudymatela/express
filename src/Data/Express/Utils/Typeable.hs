-- |
-- Module      : Data.Express.Utils.Typeable
-- Copyright   : (c) 2016-2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Express.
--
-- Utilities to manipulate 'TypeRep's (of 'Typeable' values).
module Data.Express.Utils.Typeable
  ( tyArity
  , unFunTy
  , isFunTy
  , argumentTy
  , resultTy
  , finalResultTy
  , boolTy
  , intTy
  , orderingTy
  , mkComparisonTy
  , mkCompareTy
  , funTyCon
  , compareTy
  , elementTy
  , (->::)
  , module Data.Typeable
  )
where

import Data.Typeable
import Data.Monoid ((<>))

-- Different versions of Typeable/GHC provide different orderings for TypeReps.
-- The following is a version independent ordering, with the following
-- properties:
--
-- * functional types with more arguments are larger;
-- * type constructors with more arguments are larger.
compareTy :: TypeRep -> TypeRep -> Ordering
compareTy t1 t2 | t1 == t2 = EQ -- optional optimization
compareTy t1 t2 = tyArity t1 `compare` tyArity t2
               <> length ts1 `compare` length ts2
               <> show c1 `compare` show c2
               <> foldr (<>) EQ (zipWith compareTy ts1 ts2)
  where
  (c1,ts1) = splitTyConApp t1
  (c2,ts2) = splitTyConApp t2

tyArity :: TypeRep -> Int
tyArity t
  | isFunTy t = 1 + tyArity (resultTy t)
  | otherwise = 0

finalResultTy :: TypeRep -> TypeRep
finalResultTy t
  | isFunTy t = finalResultTy (resultTy t)
  | otherwise = t

unFunTy :: TypeRep -> (TypeRep,TypeRep)
unFunTy t
  | isFunTy t = let (f,[a,b]) = splitTyConApp t in (a,b)
  | otherwise = error $ "error (unFunTy): `" ++ show t ++ "` is not a function type"

argumentTy :: TypeRep -> TypeRep
argumentTy = fst . unFunTy

resultTy :: TypeRep -> TypeRep
resultTy = snd . unFunTy

-- | This function returns the type of the element of a list.
--   It will throw an error when not given the list type.
--
--   > > > elementTy $ typeOf (undefined :: [Int])
--   > Int
--   > > > elementTy $ typeOf (undefined :: [[Int]])
--   > [Int]
--   > > > elementTy $ typeOf (undefined :: [Bool])
--   > Bool
--   > > > elementTy $ typeOf (undefined :: Bool)
--   > *** Exception: error (elementTy): `Bool' is not a list type
elementTy :: TypeRep -> TypeRep
elementTy t
  | isListTy t = let (_,[a]) = splitTyConApp t in a
  | otherwise = error $ "error (elementTy): `" ++ show t ++ "' is not a list type"

boolTy :: TypeRep
boolTy = typeOf (undefined :: Bool)

intTy :: TypeRep
intTy = typeOf (undefined :: Int)

orderingTy :: TypeRep
orderingTy = typeOf (undefined :: Ordering)

funTyCon :: TyCon
funTyCon = typeRepTyCon $ typeOf (undefined :: () -> ())

listTyCon :: TyCon
listTyCon = typeRepTyCon $ typeOf (undefined :: [()])

isFunTy :: TypeRep -> Bool
isFunTy t =
  case splitTyConApp t of
    (con,[_,_]) | con == funTyCon -> True
    _ -> False

isListTy :: TypeRep -> Bool
isListTy t  =  case splitTyConApp t of
  (con,[_]) | con == listTyCon -> True
  _ -> False

mkComparisonTy :: TypeRep -> TypeRep
mkComparisonTy a = a ->:: a ->:: boolTy

mkCompareTy :: TypeRep -> TypeRep
mkCompareTy a = a ->:: a ->:: orderingTy

-- | An infix alias for 'mkFunTy'.  It is right associative.
(->::) :: TypeRep -> TypeRep -> TypeRep
(->::) = mkFunTy
infixr 9 ->::
