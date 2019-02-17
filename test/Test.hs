module Test
  ( module Test.LeanCheck
  , module Test.LeanCheck.Utils
  , module Data.Haexpress.Fixtures
  , module Test.ListableExpr
  , module Data.Maybe
  , mainTest
  )
where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (elemIndices)

import Data.Maybe
import Test.LeanCheck
import Test.LeanCheck.Utils
import Test.ListableExpr
import Data.Haexpress.Fixtures

reportTests :: [Bool] -> IO ()
reportTests tests =
  case elemIndices False tests of
    [] -> putStrLn "+++ Tests passed!"
    is -> do putStrLn ("*** Failed tests:" ++ show is)
             exitFailure

getMaxTestsFromArgs :: Int -> IO Int
getMaxTestsFromArgs n = do
  as <- getArgs
  return $ case as of
             (s:_) -> read s
             _     -> n

mainTest :: (Int -> [Bool]) -> Int -> IO ()
mainTest tests n' = do
  n <- getMaxTestsFromArgs n'
  reportTests (tests n)
