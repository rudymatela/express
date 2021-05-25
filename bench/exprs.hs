-- exprs.hs -- how long it takes to enumerate expressions?
--
-- Copyright (c) 2021 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- This program intentionally has the same parameters as:
-- * bench/match.hs
-- * bench/triexpr.hs
import Test



showEq :: (Expr, Expr) -> String
showEq (lhs, rhs)  =  showExpr lhs ++ "  =  " ++ showExpr rhs

exprs :: [Expr]
exprs  =  take 110880 list




main :: IO ()
main  =  do
  putStrLn $ unlines $ map showEq $ allRules
  putStrLn $ unlines $ map show $ take 1080 $ exprs
  print $ (== ']') $ last $ show exprs
