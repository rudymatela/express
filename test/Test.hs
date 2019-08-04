-- |
-- Module      : Test
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module defines utilities used to test "Data.Express".
--
-- It should never be exported in @ express.cabal @.
module Test
  ( module Test.LeanCheck
  , module Test.LeanCheck.Utils
  , module Test.LeanCheck.Derive
  , module Data.Express.Fixtures
  , module Data.Express.Utils.List
  , module Data.Express.Utils.Typeable
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
import Test.LeanCheck.Derive
import Test.ListableExpr
import Data.Express.Fixtures
import Data.Express.Utils.List
import Data.Express.Utils.Typeable

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
