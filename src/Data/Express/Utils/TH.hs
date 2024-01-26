{-# LANGUAGE TemplateHaskell, CPP #-}
-- |
-- Module      : Data.Express.Name.Derive
-- Copyright   : (c) 2019-2024 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Template Haskell utilities.
module Data.Express.Utils.TH
  ( reallyDeriveCascading
  , deriveWhenNeeded
  , deriveWhenNeededOrWarn
  , typeConArgs
  , typeConArgsThat
  , typeConCascadingArgsThat
  , normalizeType
  , normalizeTypeUnits
  , isInstanceOf
  , isntInstanceOf
  , typeArity
  , typeConstructors
  , isTypeSynonym
  , typeSynonymType
  , mergeIFns
  , mergeI
  , lookupValN
  , showJustName
  , typeConstructorsArgNames
  , (|=>|)
  , (|++|)
  , whereI
  , unboundVars
  , toBounded
  , toBoundedQ
  , module Language.Haskell.TH
  )
where

import Control.Monad
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Lib

deriveWhenNeeded :: Name -> (Name -> DecsQ) -> Name -> DecsQ
deriveWhenNeeded  =  deriveWhenNeededX False

deriveWhenNeededOrWarn :: Name -> (Name -> DecsQ) -> Name -> DecsQ
deriveWhenNeededOrWarn  =  deriveWhenNeededX True

deriveWhenNeededX :: Bool -> Name -> (Name -> DecsQ) -> Name -> DecsQ
deriveWhenNeededX warnExisting cls reallyDerive t  =  do
  is <- t `isInstanceOf` cls
  if is
  then do
    unless (not warnExisting)
      (reportWarning $ "Instance " ++ showJustName cls ++ " " ++ showJustName t
                    ++ " already exists, skipping derivation")
    return []
  else
    reallyDerive t

-- |
-- Encodes a 'Name' as a 'String'.
-- This is useful when generating error messages.
--
-- > > showJustName ''Int
-- > "Int"
--
-- > > showJustName ''String
-- > "String"
--
-- > > showJustName ''Maybe
-- > "Maybe"
showJustName :: Name -> String
showJustName = reverse . takeWhile (/= '.') . reverse . show

reallyDeriveCascading :: Name -> (Name -> DecsQ) -> Name -> DecsQ
reallyDeriveCascading cls reallyDerive t =
      return . concat
  =<< mapM reallyDerive
  =<< filterM (liftM not . isTypeSynonym)
  =<< return . (t:) . delete t
  =<< t `typeConCascadingArgsThat` (`isntInstanceOf` cls)

typeConArgs :: Name -> Q [Name]
typeConArgs t  =  do
  is <- isTypeSynonym t
  if is
  then typeConTs `fmap` typeSynonymType t
  else (nubMerges . map typeConTs . concatMap snd) `fmap` typeConstructors t
  where
  typeConTs :: Type -> [Name]
  typeConTs (AppT t1 t2)  =  typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (SigT t _)  =  typeConTs t
  typeConTs (VarT _)  =  []
  typeConTs (ConT n)  =  [n]
#if __GLASGOW_HASKELL__ >= 800
  -- typeConTs (PromotedT n)  =  [n] ?
  typeConTs (InfixT  t1 n t2)  =  typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (UInfixT t1 n t2)  =  typeConTs t1 `nubMerge` typeConTs t2
  typeConTs (ParensT t)  =  typeConTs t
#endif
  typeConTs _  =  []

typeConArgsThat :: Name -> (Name -> Q Bool) -> Q [Name]
t `typeConArgsThat` p  =  filterM p =<< typeConArgs t

typeConCascadingArgsThat :: Name -> (Name -> Q Bool) -> Q [Name]
t `typeConCascadingArgsThat` p  =  do
  ts <- t `typeConArgsThat` p
  let p' t'  =  (t' `notElem` t:ts &&) `fmap` p t'
  tss <- mapM (`typeConCascadingArgsThat` p') ts
  return $ nubMerges (ts:tss)

-- |
-- Normalizes a type by applying it to necessary type variables
-- making it accept zero type parameters.
-- The normalized type is paired with a list of necessary type variables.
--
-- > > putStrLn $(stringE . show =<< normalizeType ''Int)
-- > (ConT ''Int, [])
--
-- > > putStrLn $(stringE . show =<< normalizeType ''Maybe)
-- > (AppT (ConT ''Maybe) (VarT ''a),[VarT ''a])
--
-- > > putStrLn $(stringE . show =<< normalizeType ''Either)
-- > (AppT (AppT (ConT ''Either) (VarT ''a)) (VarT ''b),[VarT ''a,VarT ''b])
--
-- > > putStrLn $(stringE . show =<< normalizeType ''[])
-- > (AppT (ConT ''[]) (VarT a),[VarT a])
normalizeType :: Name -> Q (Type, [Type])
normalizeType t  =  do
  ar <- typeArity t
  vs <- newVarTs ar
  return (foldl AppT (ConT t) vs, vs)
  where
    newNames :: [String] -> Q [Name]
    newNames  =  mapM newName
    newVarTs :: Int -> Q [Type]
    newVarTs n  =  map VarT
            `fmap` newNames (take n . map (:[]) $ cycle ['a'..'z'])

-- |
-- Normalizes a type by applying it to units to make it star-kinded.
-- (cf. 'normalizeType')
--
-- > normalizeTypeUnits ''Int    === [t| Int |]
-- > normalizeTypeUnits ''Maybe  === [t| Maybe () |]
-- > normalizeTypeUnits ''Either === [t| Either () () |]
normalizeTypeUnits :: Name -> Q Type
normalizeTypeUnits t  =  do
  ar <- typeArity t
  return (foldl AppT (ConT t) (replicate ar (TupleT 0)))

-- |
-- Given a type name and a class name,
-- returns whether the type is an instance of that class.
-- The given type must be star-kinded (@ * @)
-- and the given class double-star-kinded (@ * -> * @.
--
-- > > putStrLn $(stringE . show =<< ''Int `isInstanceOf` ''Num)
-- > True
--
-- > > putStrLn $(stringE . show =<< ''Int `isInstanceOf` ''Fractional)
-- > False
isInstanceOf :: Name -> Name -> Q Bool
isInstanceOf tn cl  =  do
  ty <- normalizeTypeUnits tn
  isInstance cl [ty]

-- |
-- The negation of 'isInstanceOf'.
isntInstanceOf :: Name -> Name -> Q Bool
isntInstanceOf tn  =  fmap not . isInstanceOf tn

-- | Given a type name, return the number of arguments taken by that type.
-- Examples in partially broken TH:
--
-- > > putStrLn $(stringE . show =<< typeArity ''Int)
-- > 0
--
-- > > putStrLn $(stringE . show =<< typeArity ''Maybe)
-- > 1
--
-- > > putStrLn $(stringE . show =<< typeArity ''Either)
-- > 2
--
-- > > putStrLn $(stringE . show =<< typeArity ''[])
-- > 1
--
-- > > putStrLn $(stringE . show =<< typeArity ''(,))
-- > 2
--
-- > > putStrLn $(stringE . show =<< typeArity ''(,,))
-- > 3
--
-- > > putStrLn $(stringE . show =<< typeArity ''String)
-- > 0
--
-- This works for data and newtype declarations and
-- it is useful when generating typeclass instances.
typeArity :: Name -> Q Int
typeArity t  =  fmap arity $ reify t
  where
  arity  =  length . args
#if __GLASGOW_HASKELL__ < 800
  args (TyConI (DataD    _ _ ks   _ _))  =  ks
  args (TyConI (NewtypeD _ _ ks   _ _))  =  ks
#else
  args (TyConI (DataD    _ _ ks _ _ _))  =  ks
  args (TyConI (NewtypeD _ _ ks _ _ _))  =  ks
#endif
  args (TyConI (TySynD _ ks _))          =  ks
  args _  =  errorOn "typeArity"
          $  "neither newtype nor data nor type synonym: " ++ show t

-- |
-- Given a type 'Name',
-- returns a list of its type constructor 'Name's
-- paired with the type arguments they take.
-- the type arguments they take.
--
-- > > putStrLn $(stringE . show =<< typeConstructors ''Bool)
-- > [ ('False, [])
-- > , ('True, [])
-- > ]
--
-- > > putStrLn $(stringE . show =<< typeConstructors ''[])
-- > [ ('[], [])
-- > , ('(:), [VarT ''a, AppT ListT (VarT ''a)])
-- > ]
--
-- > > putStrLn $(stringE . show =<< typeConstructors ''(,))
-- > [('(,), [VarT (mkName "a"), VarT (mkName "b")])]
--
-- > > data Point  =  Pt Int Int
-- > > putStrLn $(stringE . show =<< typeConstructors ''Point)
-- > [('Pt,[ConT ''Int, ConT ''Int])]
typeConstructors :: Name -> Q [(Name,[Type])]
typeConstructors t  =  fmap (map normalize . cons) $ reify t
  where
#if __GLASGOW_HASKELL__ < 800
  cons (TyConI (DataD    _ _ _   cs _))  =  cs
  cons (TyConI (NewtypeD _ _ _   c  _))  =  [c]
#else
  cons (TyConI (DataD    _ _ _ _ cs _))  =  cs
  cons (TyConI (NewtypeD _ _ _ _ c  _))  =  [c]
#endif
  cons _  =  errorOn "typeConstructors"
          $  "neither newtype nor data: " ++ show t
  normalize (NormalC n ts)   =  (n,map snd ts)
  normalize (RecC    n ts)   =  (n,map trd ts)
  normalize (InfixC  t1 n t2)  =  (n,[snd t1,snd t2])
  normalize _  =  errorOn "typeConstructors"
               $  "unexpected unhandled case when called with " ++ show t
  trd (x,y,z)  =  z

-- |
-- Is the given 'Name' a type synonym?
--
-- > > putStrLn $(stringE . show =<< isTypeSynonym 'show)
-- > False
--
-- > > putStrLn $(stringE . show =<< isTypeSynonym ''Char)
-- > False
--
-- > > putStrLn $(stringE . show =<< isTypeSynonym ''String)
-- > True
isTypeSynonym :: Name -> Q Bool
isTypeSynonym  =  fmap is . reify
  where
  is (TyConI (TySynD _ _ _))  =  True
  is _                        =  False

-- |
-- Resolves a type synonym.
--
-- > > putStrLn $(stringE . show =<< typeSynonymType ''String)
-- > AppT ListT (ConT ''Char)
typeSynonymType :: Name -> Q Type
typeSynonymType t  =  fmap typ $ reify t
  where
  typ (TyConI (TySynD _ _ t'))  =  t'
  typ _  =  errorOn "typeSynonymType" $ "not a type synonym: " ++ show t

-- Append to instance contexts in a declaration.
--
-- > sequence [[|Eq b|],[|Eq c|]] |=>| [t|instance Eq a => Cl (Ty a) where f=g|]
-- > == [t| instance (Eq a, Eq b, Eq c) => Cl (Ty a) where f  =  g |]
(|=>|) :: Cxt -> DecsQ -> DecsQ
c |=>| qds  =  map (=>++ c) `fmap` qds
  where
#if __GLASGOW_HASKELL__ < 800
  (InstanceD   c ts ds) =>++ c'  =  InstanceD   (c++c') ts ds
#else
  (InstanceD o c ts ds) =>++ c'  =  InstanceD o (c++c') ts ds
#endif
  d                     =>++ _   =  d

(|++|) :: DecsQ -> DecsQ -> DecsQ
(|++|) = liftM2 (++)

mergeIFns :: DecsQ -> DecsQ
mergeIFns qds = do ds <- qds
                   return $ map m' ds
  where
#if __GLASGOW_HASKELL__ < 800
  m' (InstanceD   c ts ds) = InstanceD   c ts [foldr1 m ds]
#else
  m' (InstanceD o c ts ds) = InstanceD o c ts [foldr1 m ds]
#endif
  FunD n cs1 `m` FunD _ cs2 = FunD n (cs1 ++ cs2)

mergeI :: DecsQ -> DecsQ -> DecsQ
qds1 `mergeI` qds2 = do ds1 <- qds1
                        ds2 <- qds2
                        return $ ds1 `m` ds2
  where
#if __GLASGOW_HASKELL__ < 800
  [InstanceD   c ts ds1] `m` [InstanceD   _ _ ds2] = [InstanceD   c ts (ds1 ++ ds2)]
#else
  [InstanceD o c ts ds1] `m` [InstanceD _ _ _ ds2] = [InstanceD o c ts (ds1 ++ ds2)]
#endif

whereI :: DecsQ -> [Dec] -> DecsQ
qds `whereI` w = do ds <- qds
                    return $ map (`aw` w) ds
#if __GLASGOW_HASKELL__ < 800
  where aw (InstanceD   c ts ds) w' = InstanceD   c ts (ds++w')
        aw d                     _  = d
#else
  where aw (InstanceD o c ts ds) w' = InstanceD o c ts (ds++w')
        aw d                     _  = d
#endif

-- > nubMerge xs ys == nub (merge xs ys)
-- > nubMerge xs ys == nub (sort (xs ++ ys))
nubMerge :: Ord a => [a] -> [a] -> [a]
nubMerge [] ys  =  ys
nubMerge xs []  =  xs
nubMerge (x:xs) (y:ys) | x < y      =  x :    xs  `nubMerge` (y:ys)
                       | x > y      =  y : (x:xs) `nubMerge`    ys
                       | otherwise  =  x :    xs  `nubMerge`    ys

nubMerges :: Ord a => [[a]] -> [a]
nubMerges  =  foldr nubMerge []

typeConstructorsArgNames :: Name -> Q [(Name,[Name])]
typeConstructorsArgNames t = do
  cs <- typeConstructors t
  sequence [ do ns <- sequence [newName "x" | _ <- ts]
                return (c,ns)
           | (c,ts) <- cs ]

-- | Lookups the name of a value
--   throwing an error when it is not found.
--
-- > > putStrLn $(stringE . show =<< lookupValN "show")
-- > 'show
lookupValN :: String -> Q Name
lookupValN s = do
  mn <- lookupValueName s
  case mn of
    Just n -> return n
    Nothing -> fail $ "lookupValN: cannot find " ++ s


-- | Lists all unbound variables in a type.
--   This intentionally excludes the 'ForallT' constructor.
unboundVars :: Type -> [Name]
unboundVars (VarT n)          =  [n]
unboundVars (AppT t1 t2)      =  nubMerge (unboundVars t1) (unboundVars t2)
unboundVars (SigT t _)        =  unboundVars t
unboundVars (ForallT vs _ t)  =  unboundVars t \\ map nm vs
  where
#if __GLASGOW_HASKELL__ < 900
  nm (PlainTV n)     =  n
  nm (KindedTV n _)  =  n
#else
  nm (PlainTV n _)     =  n
  nm (KindedTV n _ _)  =  n
#endif
unboundVars _                 =  []


-- | Binds all unbound variables using a 'ForallT' constructor.
--   (cf. 'unboundVars')
toBounded :: Type -> Type
#if __GLASGOW_HASKELL__ < 900
toBounded t  =  ForallT [PlainTV n | n <- unboundVars t] [] t
#else
toBounded t  =  ForallT [PlainTV n SpecifiedSpec | n <- unboundVars t] [] t
#endif


-- | Same as toBounded but lifted over 'Q'
toBoundedQ :: TypeQ -> TypeQ
toBoundedQ  =  liftM toBounded

errorOn :: String -> String -> a
errorOn fn msg  =  error $ "Data.Express.Derive.Utils." ++ fn ++ ": " ++ msg
