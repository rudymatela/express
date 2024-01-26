-- |
-- Module      : Data.Express.Utils.Typeable
-- Copyright   : (c) 2016-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module is part of Express.
--
-- Utilities to manipulate 'TypeRep's (of 'Typeable' values).
module Data.Express.Utils.Typeable
  ( tyArity
  , unFunTy
  , isFunTy
  , argumentTy
  , resultTy
  , finalResultTy
  , boolTy
  , intTy
  , orderingTy
  , mkComparisonTy
  , mkCompareTy
  , funTyCon
  , compareTy
  , elementTy
  , typesIn
  , typesInList
  , countListTy
  , (->::)
  , module Data.Typeable
  )
where

import Data.Typeable
import Data.Express.Utils

-- | Compares two 'TypeRep's.
--
-- Different versions of Typeable/GHC
-- provide different orderings for 'TypeRep's.
-- The following is a version independent ordering,
-- with the following properties:
--
-- * functional types with more arguments are larger;
-- * type constructors with more arguments are larger.
--
-- > > typeOf (undefined :: Int -> Int) `compareTy` typeOf (undefined :: () -> () -> ())
-- > LT
--
-- > > typeOf (undefined :: Int) `compareTy` typeOf (undefined :: ())
-- > GT
compareTy :: TypeRep -> TypeRep -> Ordering
compareTy t1 t2 | t1 == t2 = EQ -- optional optimization
compareTy t1 t2 = tyArity t1 `compare` tyArity t2
               <> length ts1 `compare` length ts2
               <> showTyCon c1 `compare` showTyCon c2
               <> foldr (<>) EQ (zipWith compareTy ts1 ts2)
  where
  (c1,ts1) = splitTyConApp t1
  (c2,ts2) = splitTyConApp t2

-- | Shows a 'TyCon' consistently across different GHC versions.
--   This is needed in the implementation of `compareTy`.
--
-- On GHC <= 9.4:
--
-- > > show listTyCon
-- > "[]"
--
-- On GHC >= 9.6:
--
-- > > show listTyCon
-- > "List"
--
-- On all GHCs:
--
-- > > showTyCon listTyCon
-- > "[]"
--
-- On GHC <= 9.6:
--
-- > > show unitTyCon
-- > "()"
--
-- On GHC >= 9.8:
--
-- > > show unitTyCon
-- > "Unit"
--
-- On all GHCs:
--
-- > > showTyCon unitTyCon
-- > "()"
--
-- Further exceptions to `show :: TyCon -> String` may be added here
-- on future versions.
showTyCon :: TyCon -> String
showTyCon con
  | con == listTyCon  =  "[]"
  | con == unitTyCon  =  "()"
  | otherwise         =  show con

-- | Returns the functional arity of the given 'TypeRep'.
--
-- > > tyArity $ typeOf (undefined :: Int)
-- > 0
--
-- > > tyArity $ typeOf (undefined :: Int -> Int)
-- > 1
--
-- > > tyArity $ typeOf (undefined :: (Int,Int))
-- > 0
tyArity :: TypeRep -> Int
tyArity t
  | isFunTy t = 1 + tyArity (resultTy t)
  | otherwise = 0

-- | Returns the ultimate result type of the given 'TypeRep'.
--
-- > > finalResultTy (typeOf (undefined :: Int))
-- > Int
--
-- > > finalResultTy (typeOf (undefined :: Int -> Char))
-- > Char
--
-- > > finalResultTy (typeOf (undefined :: Int -> Char -> Bool))
-- > Bool
finalResultTy :: TypeRep -> TypeRep
finalResultTy t
  | isFunTy t = finalResultTy (resultTy t)
  | otherwise = t

-- | Deconstructs a functional 'TypeRep' into a pair of 'TypeRep's.
--
-- > > unFunTy $ typeOf (undefined :: Int -> Char -> Bool)
-- > (Int,Char -> Bool)
--
-- This function raises an error on non-functional types.
--
-- (cf. 'argumentTy' and 'resultTy')
unFunTy :: TypeRep -> (TypeRep,TypeRep)
unFunTy t
  | isFunTy t = let (f,[a,b]) = splitTyConApp t in (a,b)
  | otherwise = errorOn "unFunTy" $ "`" ++ show t ++ "' is not a function type"

-- | Returns the argument 'TypeRep' of a given functional 'TypeRep'.
--
-- > argumentTy $ typeOf (undefined :: Int -> Char)
-- > Int
--
-- This function raises an error on non-functional types.
--
-- (cf. 'resultTy')
argumentTy :: TypeRep -> TypeRep
argumentTy = fst . unFunTy

-- | Returns the result 'TypeRep' of a given functional 'TypeRep'.
--
-- > > resultTy $ typeOf (undefined :: Int -> Char)
-- > Char
--
-- > > resultTy $ typeOf (undefined :: Int -> Char -> Bool)
-- > Char -> Bool
--
-- This function raises an error on non-functional types.
--
-- (cf. 'argumentTy' and 'finalResultTy')
resultTy :: TypeRep -> TypeRep
resultTy = snd . unFunTy

-- | This function returns the type of the element of a list.
--   It will throw an error when not given the list type.
--
--   > > > elementTy $ typeOf (undefined :: [Int])
--   > Int
--   > > > elementTy $ typeOf (undefined :: [[Int]])
--   > [Int]
--   > > > elementTy $ typeOf (undefined :: [Bool])
--   > Bool
--   > > > elementTy $ typeOf (undefined :: Bool)
--   > *** Exception: Data.Express.Utils.Typeable.elementTy: `Bool' is not a list type
elementTy :: TypeRep -> TypeRep
elementTy t
  | isListTy t = let (_,[a]) = splitTyConApp t in a
  | otherwise = errorOn "elementTy" $ "`" ++ show t ++ "' is not a list type"

-- | The 'Bool' type encoded as a 'TypeRep'.
boolTy :: TypeRep
boolTy = typeOf (undefined :: Bool)

-- | The 'Int' type encoded as a 'TypeRep'.
intTy :: TypeRep
intTy = typeOf (undefined :: Int)

-- | The 'Ordering' type encoded as a 'TypeRep'.
orderingTy :: TypeRep
orderingTy = typeOf (undefined :: Ordering)

-- | The function type constructor as a 'TyCon'
funTyCon :: TyCon
funTyCon = typeRepTyCon $ typeOf (undefined :: () -> ())

-- | The list type constructor as a 'TyCon'
listTyCon :: TyCon
listTyCon = typeRepTyCon $ typeOf (undefined :: [()])

-- | The unit type constructor as a 'TyCon'
unitTyCon :: TyCon
unitTyCon = typeRepTyCon $ typeOf (undefined :: ())

-- | Returns whether a 'TypeRep' is functional.
--
-- > > isFunTy $ typeOf (undefined :: Int -> Int)
-- > True
-- > > isFunTy $ typeOf (undefined :: Int)
-- > False
isFunTy :: TypeRep -> Bool
isFunTy t =
  case splitTyConApp t of
    (con,[_,_]) | con == funTyCon -> True
    _ -> False

isListTy :: TypeRep -> Bool
isListTy t  =  case splitTyConApp t of
  (con,[_]) | con == listTyCon -> True
  _ -> False

-- | Return the number of outer list nestings in a 'TypeRep'
--
-- > > countListTy $ typeOf (undefined :: Int)
-- > 0
--
-- > > countListTy $ typeOf (undefined :: [Bool])
-- > 1
--
-- > > countListTy $ typeOf (undefined :: [[()]])
-- > 2
--
-- > > countListTy $ typeOf (undefined :: String)
-- > 1
--
-- > > countListTy $ typeOf (undefined :: ([Int],[Bool]))
-- > 0
countListTy :: TypeRep -> Int
countListTy t  =  case splitTyConApp t of
  (con,[t']) | con == listTyCon -> 1 + countListTy t'
  _ -> 0

-- | Constructs a comparison type (@ a -> a -> Bool @)
--   from the given argument type.
--
-- > > mkComparisonTy $ typeOf (undefined :: Int)
-- > Int -> Int -> Bool
--
-- > > mkComparisonTy $ typeOf (undefined :: ())
-- > () -> () -> Bool
mkComparisonTy :: TypeRep -> TypeRep
mkComparisonTy a = a ->:: a ->:: boolTy

-- | Constructs a "compare" type (@ a -> a -> Ordering @)
--   from the given argument type.
--
-- > > mkCompareTy $ typeOf (undefined :: Int)
-- > Int -> Int -> Ordering
--
-- > > mkCompareTy $ typeOf (undefined :: ())
-- > () -> () -> Ordering

mkCompareTy :: TypeRep -> TypeRep
mkCompareTy a = a ->:: a ->:: orderingTy

-- | /O(n)/.
-- Return all sub types of a given type including itself.
--
-- > > typesIn $ typeOf (undefined :: Int)
-- > [Int]
--
-- > > typesIn $ typeOf (undefined :: Bool)
-- > [Bool]
--
-- > > typesIn $ typeOf (undefined :: [Int])
-- > [ Int
-- > , [Int]
-- > ]
--
-- > > typesIn $ typeOf (undefined :: Int -> Int -> Int)
-- > [ Int
-- > , Int -> Int
-- > , Int -> Int -> Int
-- > ]
--
-- > > typesIn $ typeOf (undefined :: Int -> [Int] -> [Int])
-- > [ Int
-- > , [Int]
-- > , [Int] -> [Int]
-- > , Int -> [Int] -> [Int]
-- > ]
--
-- > > typesIn $ typeOf (undefined :: Maybe Bool)
-- > [ Bool
-- > , Maybe Bool
-- > ]
typesIn :: TypeRep -> [TypeRep]
typesIn t  =  typesInList [t]

-- | Returns types and subtypes from the given list of 'TypeRep's.
--
-- > > typesInList [typeOf (undefined :: () -> Int), typeOf (undefined :: String -> String -> Bool)]
-- > [(),Bool,Char,Int,[Char],() -> Int,[Char] -> Bool,[Char] -> [Char] -> Bool]
--
-- > > typesInList [typeOf (undefined :: (Char,Int))]
-- > [Char,Int,(Char,Int)]
typesInList :: [TypeRep] -> [TypeRep]
typesInList ts  =  nubSortBy compareTy $ tins ts []
  where
  tin t  =  (t:) . tins (typeRepArgs t)
  tins  =  foldr ((.) . tin) id

-- | An infix alias for 'mkFunTy'.  It is right associative.
(->::) :: TypeRep -> TypeRep -> TypeRep
(->::) = mkFunTy
infixr 9 ->::

errorOn :: String -> String -> a
errorOn fn msg  =  error $ "Data.Express.Utils.Typeable." ++ fn ++ ": " ++ msg
