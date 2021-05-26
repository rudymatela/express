-- triexpr.hs -- how long it takes to match expressions in a Triexpr?
--
-- Copyright (c) 2021 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
-- This program has the same parameters as other match*.hs benchmarks.
import Test
import Data.Express.Triexpr (Triexpr)
import qualified Data.Express.Triexpr as T

showEq :: (Expr, Expr) -> String
showEq (lhs, rhs)  =  showExpr lhs ++ "  =  " ++ showExpr rhs

exprs :: [Expr]
exprs  =  take 720720 list

query :: Expr -> Maybe (Expr,[(Expr,Expr)],Expr)
query e  =  listToMaybe $ T.lookup e trie

main :: IO ()
main  =  do
  putStrLn $ unlines $ map showEq $ allRules
  putStrLn $ unlines $ map show $ mapMaybe query $ take 1080 $ exprs
  print $ (== ']') $ last $ show $ mapMaybe query exprs

trie :: Triexpr Expr
trie  =  T.fromList allRules
