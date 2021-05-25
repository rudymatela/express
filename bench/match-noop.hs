-- exprs.hs -- how long it takes to enumerate expressions?
--
-- Copyright (c) 2021 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- This program has the same parameters as other match*.hs benchmarks.
import Test



showEq :: (Expr, Expr) -> String
showEq (lhs, rhs)  =  showExpr lhs ++ "  =  " ++ showExpr rhs

exprs :: [Expr]
exprs  =  take 720720 list




main :: IO ()
main  =  do
  putStrLn $ unlines $ map showEq $ allRules
  putStrLn $ unlines $ map show $ take 1080 $ exprs
  print $ (== ']') $ last $ show exprs