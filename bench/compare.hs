-- ord.hs -- prints results of the Ord Expr's instance
--
-- Copyright (c) 2019-2021 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test
import System.Environment (getArgs)

main :: IO ()
main  =  do
  as <- getArgs
  let n = case as of
          [] -> 5040
          (n:_) -> read n
  putStrLn . unlines . map showCompare $ take n (list :: [(Expr,Expr)])
  where
  showCompare (e1,e2) = concat
    [ show e1
    , " `compare` "
    , show e2
    , "  =  "
    , show $ e1 `compare` e2
    ]
