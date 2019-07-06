module Test
  ( module Test.LeanCheck
  , module Test.LeanCheck.Utils
  , module Data.Haexpress.Fixtures
  , module Data.Haexpress.Utils.List
  , module Test.ListableExpr
  , module Data.Maybe
  , module Data.Either
  , mainTest

  , tyBool
  , tyInt
  , tyChar
  , tyInts
  , tyIntToInt
  )
where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Data.List (elemIndices)
import Data.Typeable (TypeRep, typeOf)

import Data.Maybe
import Data.Either
import Test.LeanCheck
import Test.LeanCheck.Utils
import Test.ListableExpr
import Data.Haexpress.Fixtures
import Data.Haexpress.Utils.List

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

tyBool :: TypeRep
tyBool  =  typeOf (undefined :: Bool)

tyInt :: TypeRep
tyInt  =  typeOf (undefined :: Int)

tyChar  :: TypeRep
tyChar  =  typeOf (undefined :: Char)

tyInts :: TypeRep
tyInts =  typeOf (undefined :: [Int])

tyIntToInt :: TypeRep
tyIntToInt  =  typeOf (undefined :: Int -> Int)
