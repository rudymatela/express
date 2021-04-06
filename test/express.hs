-- Copyright (c) 2019-2021 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
{-# LANGUAGE CPP #-}
import Test

main :: IO ()
main = mainTest tests 5040

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ \x -> expr x == val (x :: Int)
  , holds n $ \c -> expr c == val (c :: Char)
  , holds n $ \p -> expr p == val (p :: Bool)
  , holds n $ \x -> expr x == val (x :: ())
  , fails n $ \xs -> expr xs == val (xs :: [Int])
  , holds n $ expr ([] :: [Int]) == val ([] :: [Int])
  , holds n $ \x xs -> expr (x:xs) /= val (x:xs :: [Int])

  , holds n (okExpress :: ()       -> Bool)
  , holds n (okExpress :: Bool     -> Bool)
  , holds n (okExpress :: Int      -> Bool)
  , holds n (okExpress :: Integer  -> Bool)
  , holds n (okExpress :: Char     -> Bool)
  , holds n (okExpress :: Ordering -> Bool)

  , holds n (okExpress :: [Bool]     -> Bool)
  , holds n (okExpress :: [Int]      -> Bool)
  , holds n (okExpress :: [Integer]  -> Bool)
  , holds n (okExpress :: [Char]     -> Bool)
  , holds n (okExpress :: [Ordering] -> Bool)

  , holds n (okExpress :: ((),())     -> Bool)
  , holds n (okExpress :: (Bool,Bool) -> Bool)
  , holds n (okExpress :: (Int,Int)   -> Bool)
  , holds n (okExpress :: ((),Bool)   -> Bool)
  , holds n (okExpress :: (Bool,Int)  -> Bool)

  , holds n (okExpress :: Maybe ()   -> Bool)
  , holds n (okExpress :: Maybe Bool -> Bool)
  , holds n (okExpress :: Maybe Int  -> Bool)

  , holds n (okExpress :: Either () ()     -> Bool)
  , holds n (okExpress :: Either Bool Bool -> Bool)
  , holds n (okExpress :: Either Int Int   -> Bool)
  , holds n (okExpress :: Either () Bool   -> Bool)
  , holds n (okExpress :: Either Bool Int  -> Bool)

  , holds n (okExpress :: (Int,Int,Int) -> Bool)
  , holds n (okExpress :: (Int,Int,Int,Int) -> Bool)
  , holds n (okExpress :: (Int,Int,Int,Int,Int) -> Bool)
  , holds n (okExpress :: (Int,Int,Int,Int,Int,Int) -> Bool)
  , holds n (okExpress :: (Int,Int,Int,Int,Int,Int,Int) -> Bool)
#if __GLASGOW_HASKELL__ < 710
-- No 8-tuples for you:
-- On GHC 7.8, 8-tuples are not Typeable instances.  We could add a standalone
-- deriving clause, but that may cause trouble if some other library does the
-- same.  User should declare Generalizable 8-tuples manually when using GHC <=
-- 7.8.
#else
  , holds n (okExpress :: (Int,Int,Int,Int,Int,Int,Int,Int) -> Bool)
  , holds n (okExpress :: (Int,Int,Int,Int,Int,Int,Int,Int,Int) -> Bool)
  , holds n (okExpress :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) -> Bool)
  , holds n (okExpress :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) -> Bool)
  , holds n (okExpress :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) -> Bool)
#endif

  -- Transforming lists into Exprs
  , expr ([]::[Int]) == value "[]" ([]::[Int])
  , expr ([0::Int])  == zero -:- nil
  , expr ([0::Int,1])  == zero -:- one -:- nil
  , holds n $ \xs -> expr xs == foldr (-:-) nil (map expr (xs :: [Int]))
  , holds n $ \ps -> expr ps == foldr (-:-) nilBool (map expr (ps :: [Bool]))

  -- Transforming Maybes into Exprs
  , expr (Nothing    :: Maybe Int)   ==  nothing
  , expr (Nothing    :: Maybe Bool)  ==  nothingBool
  , expr (Just 1     :: Maybe Int)   ==  just one
  , expr (Just False :: Maybe Bool)  ==  just false
  , holds n $ \x -> expr (Just x) == just (expr (x :: Int))
  , holds n $ \p -> expr (Just p) == just (expr (p :: Bool))

  -- Transforming Tuples into Exprs
  , expr ((0,False) :: (Int,Bool))  ==  pair zero false
  , expr ((True,1)  :: (Bool,Int))  ==  pair true one

  -- Showing Exprs
  , holds n $ \x -> show (expr x) == show (x :: ()) ++ " :: ()"
  , holds n $ \x -> show (expr x) == show (x :: Int) ++ " :: Int"
  , holds n $ \p -> show (expr p) == show (p :: Bool) ++ " :: Bool"
  , holds n $ \x -> show (expr x) == show (x :: ((),Maybe Int,[Bool]))
                                        ++ " :: ((),(Maybe Int),[Bool])"
  ]

-- this is true only for some types
exprIsVal :: (Listable a, Express a, Show a) => a -> Bool
exprIsVal x  =  expr x == val x

-- this should be true for all types
exprIsValUnderEvaluate :: (Listable a, Express a, Show a, Eq a) => a -> Bool
exprIsValUnderEvaluate x  =  evaluate (expr x) == (evaluate (val x) -: mayb x)

-- this should be true for most types
showExprIsShow :: (Listable a, Express a, Show a) => a -> Bool
showExprIsShow x  =  showExpr (expr x) == show x

okExpress :: (Listable a, Express a, Show a, Eq a) => a -> Bool
okExpress  =  exprIsValUnderEvaluate &&& showExprIsShow
