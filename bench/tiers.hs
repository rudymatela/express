-- tiers.hs -- prints tiers of expressions
--
-- Copyright (c) 2019 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import Data.List (intercalate, nub)
import Test.LeanCheck.Tiers (showTiers)
import Data.Haexpress.Utils.List

showDotsLongerThan :: Show a => Int -> [a] -> String
showDotsLongerThan n xs = "["
                       ++ intercalate "," (dotsLongerThan n $ map show xs)
                       ++ "]"
  where
  dotsLongerThan n xs = take n xs ++ ["..." | not . null $ drop n xs]

printTiers :: Show a => Int -> [[a]] -> IO ()
printTiers n = putStrLn . init . unlines . map ("  " ++) . lines . showTiers n

main :: IO ()
main  =  do
  putStrLn $ "isNub (list :: [Expr])  =  "
          ++ show (isNub (take 5040 list :: [Expr]))
  putStrLn $ "map length (tiers :: [[ Expr ]])  =  "
          ++ showDotsLongerThan 6 (map length (tiers :: [[Expr]]))
  putStrLn $ "tiers :: [[ Expr ]]  ="
  printTiers 6 (tiers :: [[Expr]])
