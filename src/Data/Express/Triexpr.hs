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

-- "Nothing" should match an App, "Just Expr" an expression
data Triexpr a = Triexpr [(Maybe Expr, Either (Triexpr a) (Expr,a))]
  deriving (Eq, Ord, Show)

empty :: Triexpr a
empty  =  Triexpr []

unit :: Expr -> a -> Triexpr a
unit e x  =  u e (Right (e,x))
  where
  u :: Expr -> (Either (Triexpr a) (Expr,a)) -> Triexpr a
  u (e1 :$ e2) et  =  Triexpr [(Nothing, Left $ u e1 $ Left $ u e2 et)]
  u e          et  =  Triexpr [(Just e,  et)]

merge :: Triexpr a -> Triexpr a -> Triexpr a
merge (Triexpr ms1) (Triexpr ms2)  =  Triexpr $ m ms1 ms2
  where
  m [] ms  =  ms
  m ms []  =  ms
  m ((e1,mt1):ms1) ((e2,mt2):ms2)
    | e1 == e2  =  case (mt1,mt2) of
                   (Left t1, Left t2) -> (e1, Left $ t1 `merge` t2) : m ms1 ms2
                   (_,_) -> (e1,mt1) : (e2,mt2) : m ms1 ms2
    | e1 <  e2  =  (e1,mt1) : m ms1 ((e2,mt2):ms2)
    | e1 >  e2  =  (e2,mt2) : m ((e1,mt1):ms1) ms2
    | otherwise =  error "merge: the impossible happened"

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

-- TODO: -> [ (Expr,[(Expr,Expr)],a) ]
lookup :: Expr -> Triexpr a -> [ (Expr, [(Expr,Expr)], a) ]
lookup e t  =  [(e, bs, x) | (bs, Right (e,x)) <- look (Just e) t []]
  where
  look :: Maybe Expr -> Triexpr a -> [(Expr, Expr)] -> [([(Expr,Expr)], Either (Triexpr a) (Expr,a))]
  look Nothing  t@(Triexpr ms) bs  =  [(bs, mt) | (Nothing, mt) <- ms]
  look (Just e) t@(Triexpr ms) bs  =  [(bs', mt) | (Just e', mt) <- ms, bs' <- maybeToList (matchLeaf e e' bs)]
                                   ++ [r | e1 :$ e2 <- [e]
                                         , (bs1, Left t1) <- look Nothing t bs
                                         , (bs2, Left t2) <- look (Just e1) t1 bs1
                                         , r              <- look (Just e2) t2 bs2]

matchLeaf :: Expr -> Expr -> [(Expr,Expr)] -> Maybe [(Expr,Expr)]
matchLeaf e1 e2
  | isVar e2 && mtyp e1 == mtyp e2  =  updateAssignments (e2,e1)
  | e1 == e2                        =  Just
  | otherwise                       =  const Nothing

updateAssignments :: (Expr,Expr) -> [(Expr,Expr)] -> Maybe [(Expr,Expr)]
updateAssignments (e,e') = \bs ->
  case P.lookup e bs of
    Nothing  -> Just ((e,e'):bs)
    Just e'' -> if e'' == e'
                then Just bs
                else Nothing
