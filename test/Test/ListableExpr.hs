module Test.ListableExpr
  (
  -- * The Expr type
    Expr

  -- * Expressions of a type
  , IntE (..)
  , FunE (..)

  -- * Value expressions
  , IntE0 (..)

  -- ** Functional values
  , FunE_II (..)
  , FunE_III (..)
  )
where

import Test.LeanCheck
import Data.Haexpress.Fixtures
import Data.Function (on)

newtype IntE0  =  IntE0 { unIntE0 :: Expr }
newtype IntEV  =  IntEV { unIntEV :: Expr }
newtype IntE   =  IntE  { unIntE  :: Expr }

newtype FunE_III  =  FunE_III { unFunE_III :: Expr }
newtype FunE_II   =  FunE_II  { unFunE_II  :: Expr }
newtype FunE      =  FunE     { unFunE     :: Expr }

instance Show IntE0 where  show (IntE0 e) = show e
instance Show IntEV where  show (IntEV e) = show e
instance Show IntE  where  show (IntE  e) = show e

-- TODO: rename to IntToInt and IntToIntToInt
instance Show FunE_III where  show (FunE_III e) = show e
instance Show FunE_II  where  show (FunE_II  e) = show e
instance Show FunE     where  show (FunE     e) = show e

-- TODO: change derivin Show above to explicitly show instance that shows inner value

instance Listable IntE0 where  tiers  =  (IntE0 . val) `mapT` (tiers :: [[Int]])
instance Listable IntEV where  list  =  map (IntEV . (`var` (undefined :: Int))) ["x", "y", "z", "x'"] -- TODO: infinite list
instance Listable IntE  where
  tiers  =  mapT IntE
         $  cons0 i_
         \/ cons1 unIntEV
         \/ cons1 unIntE0
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
