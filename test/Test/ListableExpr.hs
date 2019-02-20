module Test.ListableExpr
  (
  -- * The Expr type
    Expr

  -- * Expressions of a type
  , IntE (..)

  , IntE0 (..)
  , IntEV (..)

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

newtype IntE0  =  IntE0 { unIntE0 :: Expr }
newtype IntEV  =  IntEV { unIntEV :: Expr }

newtype IntToIntE  =  IntToIntE { unIntToIntE :: Expr }
newtype IntToIntToIntE  =  IntToIntToIntE { unIntToIntToIntE :: Expr }


instance Show IntE  where  show (IntE e) = show e

instance Show IntE0 where  show (IntE0 e) = show e
instance Show IntEV where  show (IntEV e) = show e

instance Show IntToIntE  where  show (IntToIntE e) = show e
instance Show IntToIntToIntE  where  show (IntToIntToIntE e) = show e

instance Listable IntE  where
  tiers  =  mapT IntE
         $  cons0 i_
         \/ cons1 unIntEV
         \/ cons1 unIntE0
         \/ cons2 (\(IntToIntE f) (IntE xx) -> f :$ xx)

instance Listable IntE0 where
  tiers  =  (IntE0 . val) `mapT` (tiers :: [[Int]])

instance Listable IntEV where
  list  =  map (IntEV . (`var` (undefined :: Int))) ["x", "y", "z", "x'"] -- TODO: infinite list

instance Listable IntToIntE where
  tiers  =  mapT IntToIntE
         $  cons0 idE
         \/ cons0 negateE `addWeight` 1
         \/ cons0 absE    `addWeight` 1
         \/ cons2 (\(IntToIntToIntE ef) (IntE ex) -> ef :$ ex)

instance Listable IntToIntToIntE where
  list  =  map IntToIntToIntE [plusE, timesE]


instance Listable Expr where
  tiers  =  reset (cons1 unIntE)
         \/ cons1 unIntToIntE      `addWeight` 1
         \/ cons1 unIntToIntToIntE `addWeight` 1


instance ShowFunction Expr where bindtiers  =  bindtiersShow
