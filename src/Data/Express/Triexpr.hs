-- |
-- Module      : Data.Express.Triexpr
-- Copyright   : (c) 2019-2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Express.
--
-- An experimental data structure for matching 'Expr's.
--
-- Take care when importing this module,
-- the interface is experimental
-- and may change at every minor version.
--
-- This module should be imported qualified
-- as it exports definitions called
-- 'map', 'lookup', 'toList', 'fromList', 'insert' and 'empty'.
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Express.Triexpr
  ( Triexpr (..)
  , empty
  , unit
  , merge
  , insert
  , toList
  , fromList
  , map
  , lookup
  )
where

import Data.Express
import Data.Maybe
import Prelude hiding (map, lookup)
import qualified Prelude as P
import Data.Typeable


-- A type tagged expression to avoid recomputing
data Tygged = Atom TypeRep Expr
            | Appl TypeRep Expr Tygged Tygged

-- A type error
data Terror = Terror deriving (Show, Typeable)

terror :: TypeRep
terror  =  typ (val Terror)

tt :: Tygged -> TypeRep
tt (Atom ty _)  =  ty
tt (Appl ty _ _ _)  =  ty

te :: Tygged -> Expr
te (Atom _ e)      =  e
te (Appl _ e _ _)  =  e

tyg :: Expr -> Tygged
tyg e@(e1 :$ e2)  =  Appl ty e tyg1 tyg2
  where
  ty  =  case tt tyg1 `funResultTy` tt tyg2 of
         Nothing -> terror
         Just ty -> ty
  tyg1  =  tyg e1
  tyg2  =  tyg e2
tyg e  =  Atom (typ e) e


-- "Left TypeRep" should match an App, "Right Expr" an expression
data Triexpr a = Triexpr [(Either TypeRep Expr, Either (Triexpr a) (Expr,a))]
  deriving (Eq, Ord, Show)

empty :: Triexpr a
empty  =  Triexpr []

unit :: Expr -> a -> Triexpr a
unit e x  =  u e (Right (e,x))
  where
  u :: Expr -> (Either (Triexpr a) (Expr,a)) -> Triexpr a
  u e@(e1 :$ e2) et  =  Triexpr [(Left (typ e), Left $ u e1 $ Left $ u e2 et)]
  u e            et  =  Triexpr [(Right e,  et)]

merge :: Triexpr a -> Triexpr a -> Triexpr a
merge (Triexpr ms1) (Triexpr ms2)  =  Triexpr $ m ms1 ms2
  where
  m [] ms  =  ms
  m ms []  =  ms
  m ((e1,mt1):ms1) ((e2,mt2):ms2) = case compare e1 e2 of
    EQ -> case (mt1,mt2) of
          (Left t1, Left t2) -> (e1, Left $ t1 `merge` t2) : m ms1 ms2
          (_,_) -> (e1,mt1) : (e2,mt2) : m ms1 ms2
    LT -> (e1,mt1) : m ms1 ((e2,mt2):ms2)
    GT -> (e2,mt2) : m ((e1,mt1):ms1) ms2

insert :: Expr -> a -> Triexpr a -> Triexpr a
insert e x t  =  unit e x `merge` t

toList :: Triexpr a -> [(Expr, a)]
toList (Triexpr ms)  =  concatMap to ms
  where
  to (_, Right ex)  =  [ex]
  to (_, Left t)  =  toList t

fromList :: [(Expr, a)] -> Triexpr a
fromList  =  foldr (uncurry insert) empty

map :: (a -> b) -> Triexpr a -> Triexpr b
map f (Triexpr ms)  =  Triexpr [(ex, mapEither (map f) (mapSnd f) eth) | (ex, eth) <- ms]
  where
  mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
  mapEither f g (Left x)   =  Left (f x)
  mapEither f g (Right y)  =  Right (g y)
  mapSnd :: (a -> b) -> (c,a) -> (c,b)
  mapSnd f (x,y)  =  (x, f y)

lookup :: Expr -> Triexpr a -> [ (Expr, [(Expr,Expr)], a) ]
lookup e t  =  [(e, bs, x) | (bs, Right (e,x)) <- look (Right $ tyg e) t []]
  where
  look :: Either TypeRep Tygged -> Triexpr a -> [(Expr, Expr)] -> [([(Expr,Expr)], Either (Triexpr a) (Expr,a))]
  look (Left ty) t@(Triexpr ms) bs  =  [(bs, mt) | (Left ty', mt) <- ms, ty' == ty]
  look (Right e) t@(Triexpr ms) bs  =  [(bs', mt) | (Right e', mt) <- ms, bs' <- maybeToList (matchLeaf (tt e) (te e) e' bs)]
                                    ++ [r | Appl ty _ e1 e2 <- [e]
                                          , (bs1, Left t1) <- look (Left ty) t bs
                                          , (bs2, Left t2) <- look (Right e1) t1 bs1
                                          , r              <- look (Right e2) t2 bs2]

matchLeaf :: TypeRep -> Expr -> Expr -> [(Expr,Expr)] -> Maybe [(Expr,Expr)]
matchLeaf t1 e1 e2
  | isVar e2 && t1 == typ e2  =  updateAssignments (e2,e1)
  | e1 == e2                  =  Just
  | otherwise                 =  const Nothing

updateAssignments :: (Expr,Expr) -> [(Expr,Expr)] -> Maybe [(Expr,Expr)]
updateAssignments (e,e') = \bs ->
  case P.lookup e bs of
    Nothing  -> Just ((e,e'):bs)
    Just e'' -> if e'' == e'
                then Just bs
                else Nothing
