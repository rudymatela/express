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

newtype IntE0  =  IntE0 { unIntE0 :: Expr } deriving Show
newtype IntE   =  IntE  { unIntE  :: Expr } deriving Show

newtype FunE_III  =  FunE_III { unFunE_III :: Expr } deriving Show
newtype FunE_II   =  FunE_II  { unFunE_II  :: Expr } deriving Show
newtype FunE      =  FunE     { unFunE     :: Expr } deriving Show

-- TODO: change derivin Show above to explicitly show instance that shows inner value
-- TODO: add variables

instance Listable IntE0 where  tiers  =  (IntE0 . val) `mapT` (tiers :: [[Int]])
instance Listable IntE  where
  tiers  =  mapT IntE
         $  cons1 unIntE0
         \/ cons2 (\(FunE_II f) (IntE xx) -> f :$ xx)
         \/ cons3 (\(FunE_III f) (IntE xx) (IntE yy) -> f :$ xx :$ yy)

instance Listable FunE_III where  list   =  map FunE_III [plusE, timesE]
instance Listable FunE_II  where  list   =  map FunE_II [idE, negateE, absE]
instance Listable FunE     where  tiers  =  mapT FunE
                                         $  cons1 unFunE_II
                                         \/ cons1 unFunE_III

instance Listable Expr where
  tiers  =  cons1 unIntE
        \/  cons1 unFunE `addWeight` 1
