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

-- "Nothing" should match an App, "Just Expr" an expression
data Triexpr a = Triexpr [(Maybe Expr, Either (Triexpr a) a)]
  deriving (Eq, Ord, Show)

empty :: Triexpr a
empty  =  Triexpr []

unit :: Expr -> a -> Triexpr a
unit e x  =  u e (Right x)
  where
  u :: Expr -> (Either (Triexpr a) a) -> Triexpr a
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
toList t  =  [(e,x) | (e, Right x) <- to t]
  where
  to :: Triexpr a -> [(Expr, Either (Triexpr a) a)]
  to (Triexpr ms)  =  [ (e,et) | (Just e, et) <- ms ]
                   ++ [ (e1 :$ e2, et) | (Nothing, Left t) <- ms
                                       , (e1, Left t')     <- to t
                                       , (e2, et)          <- to t' ]

fromList :: [(Expr, a)] -> Triexpr a
fromList  =  foldr (uncurry insert) empty

map :: (a -> b) -> Triexpr a -> Triexpr b
map f (Triexpr ms)  =  Triexpr [(ex, mapEither (map f) f eth) | (ex, eth) <- ms]
  where
  mapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
  mapEither f g (Left x)   =  Left (f x)
  mapEither f g (Right y)  =  Right (g y)

-- TODO: -> [ (Expr,[(Expr,Expr)],a) ]
lookup :: Expr -> Triexpr a -> [ ([(Expr,Expr)], a) ]
lookup e t  =  [(bs, x) | (bs, Right x) <- look (Just e) t []]
  where
  look :: Maybe Expr -> Triexpr a -> [(Expr, Expr)] -> [([(Expr,Expr)], Either (Triexpr a) a)]
  look Nothing  t@(Triexpr ms) bs  =  [(bs, mt) | (Nothing, mt) <- ms]
  look (Just e) t@(Triexpr ms) bs  =  [(bs', mt) | (Just e', mt) <- ms, bs' <- maybeToList (matchWith bs e e')]
                                   ++ [r | e1 :$ e2 <- [e]
                                         , (bs1, Left t1) <- look Nothing t bs
                                         , (bs2, Left t2) <- look (Just e1) t1 bs1
                                         , r              <- look (Just e2) t2 bs2]
