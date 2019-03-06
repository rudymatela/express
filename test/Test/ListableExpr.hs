module Test.ListableExpr
  (
  -- * The Expr type
    Expr

  -- * Expressions of a type
  , IntE (..)
  , BoolE (..)
  , IntsE (..)

  , IntE0 (..)
  , IntEV (..)
  , BoolE0 (..)
  , BoolEV (..)
  , IntsE0 (..)
  , IntsEV (..)

  -- ** Functional values
  , IntToIntE (..)
  , IntToIntToIntE (..)
  , BoolToBoolE (..)
  , BoolToBoolToBoolE (..)

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

-- | Expression of 'Bool' type.
newtype BoolE  =  BoolE { unBoolE :: Expr }

-- | Constant terminal value of 'Bool' type.
newtype BoolE0  =  BoolE0 { unBoolE0 :: Expr }

-- | Varialbe of 'Bool' type.
newtype BoolEV  =  BoolEV { unBoolEV :: Expr }

-- | Functions from Bool to Bool
newtype BoolToBoolE  =  BoolToBoolE { unBoolToBoolE :: Expr }
newtype BoolToBoolToBoolE  =  BoolToBoolToBoolE { unBoolToBoolToBoolE :: Expr }

-- | Ill typed expressions.
newtype Ill  =  Ill { unIll :: Expr }


instance Show E0  where  show (E0 e) = show e
instance Show EV  where  show (EV e) = show e

instance Show IntE  where  show (IntE e) = show e

instance Show IntE0  where  show (IntE0 e) = show e
instance Show IntEV  where  show (IntEV e) = show e

instance Show IntToIntE  where  show (IntToIntE e) = show e
instance Show IntToIntToIntE  where  show (IntToIntToIntE e) = show e

instance Show BoolE  where  show (BoolE e) = show e

instance Show BoolE0  where  show (BoolE0 e) = show e
instance Show BoolEV  where  show (BoolEV e) = show e

instance Show BoolToBoolE  where  show (BoolToBoolE e) = show e
instance Show BoolToBoolToBoolE  where  show (BoolToBoolToBoolE e) = show e

instance Show IntsE  where  show (IntsE e) = show e

instance Show IntsE0  where  show (IntsE0 e) = show e
instance Show IntsEV  where  show (IntsEV e) = show e

-- | Expression of 'Ints' type.
newtype IntsE  =  IntsE { unIntsE :: Expr }

-- | Constant terminal value of 'Ints' type.
newtype IntsE0  =  IntsE0 { unIntsE0 :: Expr }

-- | Varialbe of 'Ints' type.
newtype IntsEV  =  IntsEV { unIntsEV :: Expr }

instance Show Ill where  show (Ill e) = show e

instance Listable IntE  where
  tiers  =  mapT IntE
         $  cons0 i_
         \/ cons1 unIntEV
         \/ cons1 unIntE0
         \/ cons2 (\(IntToIntE f) (IntE xx) -> f :$ xx)
         \/ cons1 (head' . unIntsE) `ofWeight` 2

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
         \/ toTiers (map (`var` (undefined :: Int -> Int)) ["f", "g", "h", "f'"]) `addWeight` 2 -- TODO: infinite list

instance Listable IntToIntToIntE where
  list  =  map IntToIntToIntE [plusE, timesE]

instance Listable IntsE  where
  tiers  =  mapT IntsE
         $  cons0 is_
         \/ cons1 unIntsEV
         \/ cons1 unIntsE0
         \/ cons2 (\(IntE ex) (IntsE exs) -> ex -:- exs)
         \/ cons1 (tail' . unIntsE) `ofWeight` 2

instance Listable IntsE0 where
  tiers  =  (IntsE0 . val) `mapT` (tiers :: [[ [Int] ]])

instance Listable IntsEV where
  list  =  map (IntsEV . (`var` (undefined :: [Int]))) ["xs", "ys", "zs", "xs'"] -- TODO: infinite list

instance Listable BoolE  where
  tiers  =  mapT BoolE
         $  cons0 b_
         \/ cons1 unBoolEV
         \/ cons1 unBoolE0
         \/ cons2 (\(BoolToBoolE ef) (BoolE ep) -> ef :$ ep)

instance Listable BoolE0 where
  tiers  =  (BoolE0 . val) `mapT` (tiers :: [[Bool]])

instance Listable BoolEV where
  list  =  map (BoolEV . (`var` (undefined :: Bool))) ["p", "q", "r", "p'"] -- TODO: infinite list

instance Listable BoolToBoolE where
  tiers  =  mapT BoolToBoolE
         $  cons0 notE
         \/ cons2 (\(BoolToBoolToBoolE ef) (BoolE ex) -> ef :$ ex)

instance Listable BoolToBoolToBoolE where
  list  =  map BoolToBoolToBoolE [orE, andE]


instance Listable E0 where
  tiers  =  mapT E0
         $  cons1 unIntE0  `ofWeight` 0
         \/ cons1 unBoolE0 `ofWeight` 1
         \/ cons1 unIntsE0 `ofWeight` 1

instance Listable EV where
  tiers  =  mapT EV
         $  cons1 unIntEV  `ofWeight` 0
         \/ cons1 unBoolEV `ofWeight` 1
         \/ cons1 unIntsEV `ofWeight` 1


instance Listable Expr where
  tiers  =  reset (cons1 unIntE)
         \/ cons1 unBoolE
         \/ cons1 unIntsE
         \/ cons1 unIntToIntE         `addWeight` 1
         \/ cons1 unIntToIntToIntE    `addWeight` 1
         \/ cons1 unBoolToBoolE       `addWeight` 2
         \/ cons1 unBoolToBoolToBoolE `addWeight` 2


-- | This listable instance only produces Ill typed expressions
instance Listable Ill where
  tiers  =  mapT Ill
         $  cons2 (\(IntE ef) (IntE ex) -> ef :$ ex) `ofWeight` 0
         \/ cons2 (\(IntToIntE ef) (IntToIntE ex) -> ef :$ ex)
         \/ cons2 (\(Ill ef) ex -> ef :$ ex)
         \/ cons2 (\ef (Ill ex)-> ef :$ ex)


instance ShowFunction Expr where bindtiers  =  bindtiersShow
