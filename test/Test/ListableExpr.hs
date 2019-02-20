module Test.ListableExpr
  (
  -- * The Expr type
    Expr

  -- * Expressions of a type
  , IntE (..)
  , FunE (..)

  -- ** Functional values
  , IntToIntE (..)
  , IntToIntToIntE (..)
  )
where

import Test.LeanCheck
import Test.LeanCheck.Function.ShowFunction
import Data.Haexpress.Fixtures
import Data.Function (on)

newtype IntE  =  IntE { unIntE :: Expr }
newtype IntToIntE  =  IntToIntE { unIntToIntE :: Expr }
newtype IntToIntToIntE  =  IntToIntToIntE { unIntToIntToIntE :: Expr }
newtype FunE  =  FunE { unFunE :: Expr }

instance Show IntE  where  show (IntE e) = show e
instance Show IntToIntE  where  show (IntToIntE e) = show e
instance Show IntToIntToIntE  where  show (IntToIntToIntE e) = show e
instance Show FunE  where  show (FunE e) = show e

instance Listable IntE  where
  tiers  =  mapT IntE
         $  cons0 i_
         \/ toTiers [var v (undefined :: Int) | v <- ["x", "y", "z", "x'"]] `addWeight` 1
         \/ mapT val (tiers :: [[Int]]) `addWeight` 1
         \/ cons2 (\(IntToIntE f) (IntE xx) -> f :$ xx)
         \/ cons3 (\(IntToIntToIntE f) (IntE xx) (IntE yy) -> f :$ xx :$ yy)

instance Listable IntToIntE where
  list  =  map IntToIntE [idE, negateE, absE]

instance Listable IntToIntToIntE where
  list  =  map IntToIntToIntE [plusE, timesE]

instance Listable FunE  where
  tiers  =  mapT FunE
         $  cons1 unIntToIntE
         \/ cons1 unIntToIntToIntE

instance Listable Expr where
  tiers  =  reset (cons1 unIntE)
         \/ cons1 unFunE `addWeight` 1

instance ShowFunction Expr where
  bindtiers  =  bindtiersShow
