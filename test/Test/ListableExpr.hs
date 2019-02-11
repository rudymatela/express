module Test.ListableExpr
  (
  -- * The Expr type
    Expr

  -- * Expressions of a type
  , IntE (..)
  , FunE (..)

  -- ** Functional values
  , FunE_II (..)
  , FunE_III (..)
  )
where

import Test.LeanCheck
import Data.Haexpress.Fixtures
import Data.Function (on)

newtype IntE   =  IntE  { unIntE  :: Expr }

newtype FunE_III  =  FunE_III { unFunE_III :: Expr }
newtype FunE_II   =  FunE_II  { unFunE_II  :: Expr }
newtype FunE      =  FunE     { unFunE     :: Expr }

instance Show IntE  where  show (IntE  e) = show e

-- TODO: rename to IntToInt and IntToIntToInt
instance Show FunE_III where  show (FunE_III e) = show e
instance Show FunE_II  where  show (FunE_II  e) = show e
instance Show FunE     where  show (FunE     e) = show e

instance Listable IntE  where
  tiers  =  mapT IntE
         $  cons0 i_
         \/ toTiers [var v (undefined :: Int) | v <- ["x", "y", "z", "x'"]] `addWeight` 1
         \/ mapT val (tiers :: [[Int]]) `addWeight` 1
         \/ cons2 (\(FunE_II f) (IntE xx) -> f :$ xx)
         \/ cons3 (\(FunE_III f) (IntE xx) (IntE yy) -> f :$ xx :$ yy)

instance Listable FunE_III where  list   =  map FunE_III [plusE, timesE]
instance Listable FunE_II  where  list   =  map FunE_II [idE, negateE, absE]
instance Listable FunE     where  tiers  =  mapT FunE
                                         $  cons1 unFunE_II
                                         \/ cons1 unFunE_III

instance Listable Expr where
  tiers  =  reset (cons1 unIntE)
        \/  cons1 unFunE `addWeight` 1
