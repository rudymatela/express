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

  -- * Terminal expressions
  , E0 (..)
  , EV (..)

  -- * Ill typed expressions
  , Ill (..)
  )
where

import Test.LeanCheck
import Test.LeanCheck.Function.ShowFunction
import Data.Haexpress.Fixtures
import Data.Function (on)

-- | Terminal constants.
newtype E0  =  E0 { unE0 :: Expr }

-- | Variables.
newtype EV  =  EV { unEV :: Expr }

-- | Expression of 'Int' type.
newtype IntE  =  IntE { unIntE :: Expr }

-- | Constant terminal value of 'Int' type.
newtype IntE0  =  IntE0 { unIntE0 :: Expr }

-- | Varialbe of 'Int' type.
newtype IntEV  =  IntEV { unIntEV :: Expr }

-- | Functions from Int to Int
newtype IntToIntE  =  IntToIntE { unIntToIntE :: Expr }
newtype IntToIntToIntE  =  IntToIntToIntE { unIntToIntToIntE :: Expr }

-- | Ill typed expressions.
newtype Ill  =  Ill { unIll :: Expr }


instance Show E0  where  show (E0 e) = show e
instance Show EV  where  show (EV e) = show e

instance Show IntE  where  show (IntE e) = show e

instance Show IntE0  where  show (IntE0 e) = show e
instance Show IntEV  where  show (IntEV e) = show e

instance Show IntToIntE  where  show (IntToIntE e) = show e
instance Show IntToIntToIntE  where  show (IntToIntToIntE e) = show e

instance Show Ill where  show (Ill e) = show e

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


instance Listable E0 where
  tiers  =  mapT E0
         $  cons1 unIntE0 `ofWeight` 0

instance Listable EV where
  tiers  =  mapT EV
         $  cons1 unIntEV `ofWeight` 0


instance Listable Expr where
  tiers  =  reset (cons1 unIntE)
         \/ cons1 unIntToIntE      `addWeight` 1
         \/ cons1 unIntToIntToIntE `addWeight` 1


-- | This listable instance only produces Ill typed expressions
instance Listable Ill where
  tiers  =  mapT Ill
         $  cons2 (\(IntE ef) (IntE ex) -> ef :$ ex) `ofWeight` 0
         \/ cons2 (\(IntToIntE ef) (IntToIntE ex) -> ef :$ ex)
         \/ cons2 (\(Ill ef) ex -> ef :$ ex)
         \/ cons2 (\ef (Ill ex)-> ef :$ ex)


instance ShowFunction Expr where bindtiers  =  bindtiersShow
