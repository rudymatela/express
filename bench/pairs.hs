-- pairs.hs -- a thousand pairs of expressions
--
-- Copyright (c) 2019-2024 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import System.Environment (getArgs)

main :: IO ()
main  =  do
  as <- getArgs
  let n = case as of
          [] -> 1000
          (n:_) -> read n
  putStrLn . unlines . map show $ take n (list :: [(Expr,Expr)])
