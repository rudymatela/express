-- u-conjure.hs -- u-Conjure
--
-- This is a prototype for Conjure, a library for conjuring code
-- out of partially implemented functions.
--
--
-- Copyright (C) 2021 Rudy Matela
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
--
--
-- To run this you need to have both LeanCheck and Express installed:
--
-- $ cabal install leancheck
-- $ cabal install express
--
-- If installation fails, use v1-install:
--
-- $ cabal v1-install leancheck
-- $ cabal v1-install express
import Data.List
import Data.Maybe
import Data.Express
import Data.Typeable
import Test.LeanCheck.Error


square :: Int -> Int
square 0  =  0
square 1  =  1
square 2  =  4
square 3  =  9
square 4  =  16

add :: Int -> Int -> Int
add 0 0  =  0
add 0 1  =  1
add 1 0  =  1
add 1 1  =  2

factorial :: Int -> Int
factorial 0  =  1
factorial 1  =  1
factorial 2  =  2
factorial 3  =  6
factorial 4  =  24

second :: [Int] -> Int
second [x,y]  =  y
second [x,y,z]  =  y
second [x,y,z,w]  =  y

-- reverse
reverse' :: [Int] -> [Int]
reverse' [x,y]  =  [y,x]
reverse' [x,y,z]  =  [z,y,x]

-- ++
(+++) :: [Int] -> [Int] -> [Int]
[x] +++ [y]  =  [x,y]
[x,y] +++ [z,w]  =  [x,y,z,w]


main :: IO ()
main  =  do
  conjure "square"    square    primitives
  conjure "add"       add       primitives
  conjure "factorial" factorial primitives

  conjure "factorial"   factorial
    [ val (0 :: Int)
    , val (1 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "*" ((*) :: Int -> Int -> Int)
    , value "foldr" (foldr :: (Int -> Int -> Int) -> Int -> [Int] -> Int)
    , value "enumFromTo" (enumFromTo :: Int -> Int -> [Int])
    ]

  conjure "second"  second   listPrimitives
  conjure "++"      (+++)    listPrimitives
  conjure "reverse" reverse' listPrimitives

  -- even by using fold and some cheating,
  -- this function is out of reach
  --  reverse xs  =  foldr (\x xs -> xs ++ [x]) [] xs
  --  reverse xs  =  foldr (flip (++) . unit) [] xs
  conjure "reverse" reverse' $ listPrimitives ++
    [ value "unit" ((:[]) :: Int -> [Int])
    , value "++" ((++) :: [Int] -> [Int] -> [Int])
    -- these last two are cheats:
    , value "flip" (flip :: ([Int]->[Int]->[Int]) -> [Int] -> [Int] -> [Int])
    , value "." ((.) :: ([Int]->[Int]->[Int]) -> (Int->[Int]) -> Int -> [Int] -> [Int])
    ]

  where

  primitives :: [Expr]
  primitives =
    [ val (0 :: Int)
    , val (1 :: Int)
    , val (2 :: Int)
    , val (3 :: Int)
    , value "+" ((+) :: Int -> Int -> Int)
    , value "*" ((*) :: Int -> Int -> Int)
    , value "-" ((-) :: Int -> Int -> Int)
    ]

  listPrimitives :: [Expr]
  listPrimitives =
    [ val (0 :: Int)
    , val (1 :: Int)
    , val ([] :: [Int])
    , value "head" (head :: [Int] -> Int)
    , value "tail" (tail :: [Int] -> [Int])
    , value ":" ((:) :: Int -> [Int] -> [Int])
    , value "foldr" (foldr :: (Int -> [Int] -> [Int]) -> [Int] -> [Int] -> [Int])
    ]


conjure :: Typeable f => String -> f -> [Expr] -> IO ()
conjure nm f primitives  =  do
  print (value nm f) -- prints the type signature
  case conjureImplementations nm f primitives of
    []    -> putStrLn $ "cannot conjure"
--  es    -> putStrLn $ unlines $ map showEq es  -- uncomment to show all found variations
    (e:_) -> putStrLn $ showEq e
  putStrLn ""
  where
  showEq eq  =  showExpr (lhs eq) ++ "  =  " ++ showExpr (rhs eq)


conjureImplementations :: Typeable f => String -> f -> [Expr] -> [Expr]
conjureImplementations nm f primitives  =
  [ appn -==- e
  | e <- candidateExprsFrom $ exs ++ primitives
  , isTrue (appn -==- e)
  ]
  where
  appn  =  application nm f primitives
  (ef:exs)  =  unfoldApp appn
  isTrue e  =  all (errorToFalse . eval False) . map (e //-) $ definedBinds appn


definedBinds :: Expr -> [[(Expr,Expr)]]
definedBinds ffxx  =  [bs | bs <- bss, errorToFalse . eval False $ e //- bs]
  where
  e  =  ffxx -==- ffxx
  bss  =  take 360 $ groundBinds ffxx


application :: Typeable f => String -> f -> [Expr] -> Expr
application nm f es  =  mostGeneralCanonicalVariation $ appn (value nm f)
  where
  appn ff | isFun ff   =  case [e | Just (_ :$ e) <- (map (ff $$)) es] of
                          [] -> error "application: could not find type representative"
                          (e:_) -> appn (ff :$ holeAsTypeOf e)
          | otherwise  =  ff


candidateExprsFrom :: [Expr] -> [Expr]
candidateExprsFrom  =  concat . take 7 . expressionsT
  where
  expressionsT ds  =  [ds] \/ (delay $ productMaybeWith ($$) es es)
    where
    es = expressionsT ds


(-==-) :: Expr -> Expr -> Expr
ex -==- ey  =  headOr (val False) . map (:$ ey) $ mapMaybe ($$ ex)
  [ value "==" ((==) :: Int -> Int -> Bool)
  , value "==" ((==) :: Bool -> Bool -> Bool)
  , value "==" ((==) :: [Int] -> [Int] -> Bool)
  , value "==" ((==) :: [Bool] -> [Bool] -> Bool)
  ]
  where
  headOr x []     =  x
  headOr _ (x:_)  =  x


lhs, rhs :: Expr -> Expr
lhs (((Value "==" _) :$ e) :$ _)  =  e
rhs (((Value "==" _) :$ _) :$ e)  =  e


groundBinds :: Expr -> [[(Expr,Expr)]]
groundBinds e  =  concat $ products [mapT ((,) v) (tiersFor v) | v <- nubVars e]


tiersFor :: Expr -> [[Expr]]
tiersFor e  =  case show (typ e) of
  "Int"    ->  mapT val (tiers `asTypeOf` [[undefined :: Int]])
  "Bool"   ->  mapT val (tiers `asTypeOf` [[undefined :: Bool]])
  "[Int]"  ->  mapT val (tiers `asTypeOf` [[undefined :: [Int]]])
  "[Bool]" ->  mapT val (tiers `asTypeOf` [[undefined :: [Bool]]])
  _        ->  []
