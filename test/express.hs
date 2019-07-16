-- Copyright (c) 2019 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
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
-- TODO: futher tuple tests
--, holds n (okExpress :: (Int,Int,Int,Int,Int,Int,Int,Int) -> Bool)
--, holds n (okExpress :: (Int,Int,Int,Int,Int,Int,Int,Int,Int) -> Bool)
--, holds n (okExpress :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) -> Bool)
--, holds n (okExpress :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) -> Bool)
--, holds n (okExpress :: (Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int,Int) -> Bool)
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
