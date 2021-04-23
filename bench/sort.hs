-- ord.hs -- prints different expression sortins:
--
-- Copyright (c) 2019-2021 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import Data.List (intercalate, nub)
import Test.LeanCheck.Tiers (showTiers)
import Data.Express.Utils.List

showL :: Show a => [a] -> String
showL xs  =  "  [ " ++ intercalate "\n  , " (map show xs) ++ "\n  ]\n"

printL :: Show a => [a] -> IO ()
printL =  putStrLn . showL

main :: IO ()
main  =  do
  putStrLn $ "sort $ take 5040 $ list  ::  [ Expr ]  ="
  printL (sort $ take 5040 $ list :: [Expr])

  putStrLn $ "sortBy compareLexicographically $ take 5040 $ list  ::  [ Expr ]  ="
  printL (sortBy compareLexicographically $ take 5040 $ list :: [Expr])

  putStrLn $ "sortBy compareQuickly $ take 5040 $ list  ::  [ Expr ]  ="
  printL (sortBy compareQuickly $ take 5040 $ list :: [Expr])
