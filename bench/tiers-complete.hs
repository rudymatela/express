-- tiers-complete.hs -- prints tiers of complete expressions
--
-- Copyright (c) 2019-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- Complete expressions are those without any holes _.
--
-- This is closely related to tiers.hs
import Test
import Data.List (intercalate, nub)
import Test.LeanCheck.Tiers (showTiers)
import Data.Express.Utils.List

showDotsLongerThan :: Show a => Int -> [a] -> String
showDotsLongerThan n xs  =  "["
                         ++ intercalate "," (dotsLongerThan n $ map show xs)
                         ++ "]"
  where
  dotsLongerThan n xs   =   take n xs ++ ["..." | not . null $ drop n xs]

printTiers :: Show a => Int -> [[a]] -> IO ()
printTiers n  =  putStrLn . init . unlines . map ("  " ++) . lines . showTiers n

main :: IO ()
main  =  do
  putStrLn $ "isNub (filter isComplete list :: [Expr])  =  "
          ++ show (isNub (take 5040 $ filter isComplete list))
  putStrLn $ "map length (filterT isComplete tiers :: [[ Expr ]])  =  "
          ++ showDotsLongerThan 11 (map length (filterT isComplete tiers :: [[Expr]]))
  putStrLn "filterT isComplete tiers :: [[ Expr ]]  ="
  printTiers 8 (filterT isComplete tiers :: [[Expr]])
