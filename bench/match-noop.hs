-- exprs.hs -- how long it takes to enumerate expressions?
--
-- Copyright (c) 2021-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- This program has the same parameters as other match*.hs benchmarks.
import Test



showEq :: (Expr, Expr) -> String
showEq (lhs, rhs)  =  showExpr lhs ++ "  =  " ++ showExpr rhs

exprs :: [Expr]
exprs  =  take 360360 list

query :: Expr -> Maybe (Expr,[(Expr,Expr)],Expr)
query e  =  Just (e,[],e)

main :: IO ()
main  =  do
  putStrLn $ unlines $ map showEq allRules
  putStrLn $ unlines $ map show $ mapMaybe query $ take 1080 exprs
  print $ (== ']') $ last $ show $ mapMaybe query exprs
