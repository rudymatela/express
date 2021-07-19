-- |
-- Module      : Data.Express.Express
-- Copyright   : (c) 2019-2021 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Express' type class.
{-# LANGUAGE CPP #-}
module Data.Express.Express
  ( Express (..)
  , (-:)
  , (->:)
  , (->>:)
  , (->>>:)
  , (->>>>:)
  , (->>>>>:)
  , (->>>>>>:)
  , (->>>>>>>:)
  , (->>>>>>>>:)
  , (->>>>>>>>>:)
  , (->>>>>>>>>>:)
  , (->>>>>>>>>>>:)
  , (->>>>>>>>>>>>:)
  )
where

import Data.Express.Core
import Data.Typeable

-- for instances
import Data.Int
import Data.Word
import Data.Ratio
import Data.Char
import Data.Complex

-- |
-- 'Express' typeclass instances provide an 'expr' function
-- that allows values to be deeply encoded as applications of 'Expr's.
--
-- > expr False  =  val False
-- > expr (Just True)  =  value "Just" (Just :: Bool -> Maybe Bool) :$ val True
--
-- The function 'expr' can be contrasted with the function 'val':
--
-- * 'val' always encodes values as atomic 'Value' 'Expr's --
--   shallow encoding.
-- * 'expr' ideally encodes expressions as applications (':$')
--   between 'Value' 'Expr's --
--   deep encoding.
--
-- Depending on the situation, one or the other may be desirable.
--
-- Instances can be automatically derived using the TH function
-- 'Data.Express.Express.Derive.deriveExpress'.
--
-- The following example shows a datatype and its instance:
--
-- > data Stack a = Stack a (Stack a) | Empty
--
-- > instance Express a => Express (Stack a) where
-- >   expr s@(Stack x y) = value "Stack" (Stack ->>: s) :$ expr x :$ expr y
-- >   expr s@Empty       = value "Empty" (Empty   -: s)
--
-- To declare 'expr' it may be useful to use auxiliary type binding operators:
-- '-:', '->:', '->>:', '->>>:', '->>>>:', '->>>>>:', ...
--
-- For types with atomic values, just declare @ expr = val @
class (Show a, Typeable a) => Express a where
  expr :: a -> Expr

instance Express ()        where  expr  =  val
instance Express Bool      where  expr  =  val
instance Express Int       where  expr  =  val
instance Express Integer   where  expr  =  val
instance Express Char      where  expr  =  val
instance Express Ordering  where  expr  =  val

instance Express a => Express (Maybe a) where
  expr mx@Nothing   =  value "Nothing" (Nothing -: mx)
  expr mx@(Just x)  =  value "Just"    (Just   ->: mx) :$ expr x

instance (Express a, Express b) => Express (Either a b) where
  expr lx@(Left x)   =  value "Left"  (Left  ->: lx) :$ expr x
  expr ry@(Right y)  =  value "Right" (Right ->: ry) :$ expr y

instance (Express a, Express b) => Express (a,b) where
  expr (x,y)  =  value "," ((,) ->>: (x,y))
              :$ expr x :$ expr y

instance (Express a, Express b, Express c) => Express (a,b,c) where
  expr (x,y,z)  =  value ",," ((,,) ->>>: (x,y,z))
                :$ expr x :$ expr y :$ expr z

instance (Express a, Express b, Express c, Express d) => Express (a,b,c,d) where
  expr (x,y,z,w)  =  value ",,," ((,,,) ->>>>: (x,y,z,w))
                  :$ expr x :$ expr y :$ expr z :$ expr w

instance Express a => Express [a] where
  expr xs  =  case xs of
              [] -> val xs
              (y:ys) -> value ":" ((:) ->>: xs) :$ expr y :$ expr ys


-- instances of further types and arities --

instance (Integral a, Express a) => Express (Ratio a) where
  expr q  =  value "%" ((%) ->>: q) :$ expr (numerator q) :$ expr (denominator q)
-- the "Integral a" restriction above is required for compilation on GHC <= 7.10

instance (RealFloat a, Express a) => Express (Complex a) where
  expr (x :+ y)  =  value ":+" ((:+) ->>: (x :+ y)) :$ expr x :$ expr y

instance (Express a, Express b, Express c, Express d, Express e)
      => Express (a,b,c,d,e) where
  expr (x,y,z,w,v)  =  value ",,,," ((,,,,) ->>>>>: (x,y,z,w,v))
                    :$ expr x :$ expr y :$ expr z :$ expr w :$ expr v

instance (Express a, Express b, Express c, Express d, Express e, Express f)
      => Express (a,b,c,d,e,f) where
  expr (x,y,z,w,v,u)  =  value ",,,,," ((,,,,,) ->>>>>>: (x,y,z,w,v,u))
                    :$ expr x :$ expr y :$ expr z :$ expr w :$ expr v :$ expr u

instance ( Express a, Express b, Express c, Express d, Express e, Express f
         , Express g )
      => Express (a,b,c,d,e,f,g) where
  expr (x,y,z,w,v,u,t)  =  value ",,,,,," ((,,,,,,) ->>>>>>>: (x,y,z,w,v,u,t))
                        :$ expr x :$ expr y :$ expr z :$ expr w
                        :$ expr v :$ expr u :$ expr t

#if __GLASGOW_HASKELL__ < 710
-- No 8-tuples for you:
-- On GHC 7.8, 8-tuples are not Typeable instances.
-- We could add a standalone deriving clause,
-- but that may cause trouble
-- if some other library does the same (orphan instance).
-- User should declare Express 8-tuples manually
-- when using GHC <= 7.8.
#else
instance ( Express a, Express b, Express c, Express d, Express e, Express f
         , Express g, Express h )
      => Express (a,b,c,d,e,f,g,h) where
  expr (x,y,z,w,v,u,t,s)  =
    value ",,,,,,," ((,,,,,,,) ->>>>>>>>: (x,y,z,w,v,u,t,s))
      :$ expr x :$ expr y :$ expr z :$ expr w
      :$ expr v :$ expr u :$ expr t :$ expr s

instance ( Express a, Express b, Express c, Express d, Express e, Express f
         , Express g, Express h, Express i )
      => Express (a,b,c,d,e,f,g,h,i) where
  expr (x,y,z,w,v,u,t,s,r)  =
    value ",,,,,,,," ((,,,,,,,,) ->>>>>>>>>: (x,y,z,w,v,u,t,s,r))
      :$ expr x :$ expr y :$ expr z :$ expr w
      :$ expr v :$ expr u :$ expr t :$ expr s
      :$ expr r

instance ( Express a, Express b, Express c, Express d, Express e, Express f
         , Express g, Express h, Express i, Express j )
      => Express (a,b,c,d,e,f,g,h,i,j) where
  expr (x,y,z,w,v,u,t,s,r,q)  =
    value ",,,,,,,,," ((,,,,,,,,,) ->>>>>>>>>>: (x,y,z,w,v,u,t,s,r,q))
      :$ expr x :$ expr y :$ expr z :$ expr w
      :$ expr v :$ expr u :$ expr t :$ expr s
      :$ expr r :$ expr q

instance ( Express a, Express b, Express c, Express d, Express e, Express f
         , Express g, Express h, Express i, Express j, Express k )
      => Express (a,b,c,d,e,f,g,h,i,j,k) where
  expr (x,y,z,w,v,u,t,s,r,q,p)  =
    value ",,,,,,,,,," ((,,,,,,,,,,) ->>>>>>>>>>>: (x,y,z,w,v,u,t,s,r,q,p))
      :$ expr x :$ expr y :$ expr z :$ expr w
      :$ expr v :$ expr u :$ expr t :$ expr s
      :$ expr r :$ expr q :$ expr p

instance ( Express a, Express b, Express c, Express d, Express e, Express f
         , Express g, Express h, Express i, Express j, Express k, Express l )
      => Express (a,b,c,d,e,f,g,h,i,j,k,l) where
  expr (x,y,z,w,v,u,t,s,r,q,p,o)  =
    value ",,,,,,,,,,," ((,,,,,,,,,,,) ->>>>>>>>>>>>: (x,y,z,w,v,u,t,s,r,q,p,o))
      :$ expr x :$ expr y :$ expr z :$ expr w
      :$ expr v :$ expr u :$ expr t :$ expr s
      :$ expr r :$ expr q :$ expr p :$ expr o
#endif

instance Express Double   where  expr  =  val
instance Express Float    where  expr  =  val
instance Express Int8     where  expr  =  val
instance Express Int16    where  expr  =  val
instance Express Int32    where  expr  =  val
instance Express Int64    where  expr  =  val
instance Express Word     where  expr  =  val
instance Express Word8    where  expr  =  val
instance Express Word16   where  expr  =  val
instance Express Word32   where  expr  =  val
instance Express Word64   where  expr  =  val
#if __GLASGOW_HASKELL__ < 710
-- No GeneralCategory for you:
-- On GHC 7.8, GeneralCategory is not a Typeable instance.
-- We could add a standalone deriving clause,
-- but that may cause trouble
-- if some other library does the same (orphan instance).
-- Users should declare their own Express GeneralCategory instance
-- when using GHC <= 7.8.
#else
instance Express GeneralCategory  where  expr  =  val
#endif


-- type binding utilities --

-- | Type restricted version of 'const'
-- that forces its first argument
-- to have the same type as the second.
--
-- >  value -: (undefined :: Ty)  =  value :: Ty
(-:) :: a -> a -> a
(-:) = asTypeOf -- const
infixl 1 -:

-- | Type restricted version of 'const'
-- that forces the result of its first argument
-- to have the same type as the second.
--
-- >  f ->: (undefined :: Ty)  =  f :: a -> Ty
(->:) :: (a -> b) -> b -> (a -> b)
(->:) = const
infixl 1 ->:

-- | Type restricted version of 'const'
-- that forces the result of the result of its first argument
-- to have the same type as the second.
--
-- > f ->>: (undefined :: Ty)  =  f :: a -> b -> Ty
(->>:) :: (a -> b -> c) -> c -> (a -> b -> c)
(->>:) = const
infixl 1 ->>:

-- | Type restricted version of 'const'
-- that forces the result of the result of the result of its first argument
-- to have the same type as the second.
(->>>:) :: (a -> b -> c -> d) -> d -> (a -> b -> c -> d)
(->>>:) = const
infixl 1 ->>>:

-- | Forces the result type of a 4-argument function.
(->>>>:) :: (a -> b -> c -> d -> e) -> e -> (a -> b -> c -> d -> e)
(->>>>:) = const
infixl 1 ->>>>:

-- | Forces the result type of a 5-argument function.
(->>>>>:) :: (a -> b -> c -> d -> e -> f) -> f -> (a -> b -> c -> d -> e -> f)
(->>>>>:) = const
infixl 1 ->>>>>:

-- | Forces the result type of a 6-argument function.
(->>>>>>:) :: (a->b->c->d->e->f->g) -> g -> (a->b->c->d->e->f->g)
(->>>>>>:) = const
infixl 1 ->>>>>>:

-- | Forces the result type of a 7-argument function.
(->>>>>>>:) :: (a->b->c->d->e->f->g->h) -> h -> (a->b->c->d->e->f->g->h)
(->>>>>>>:) = const
infixl 1 ->>>>>>>:

-- | Forces the result type of a 8-argument function.
(->>>>>>>>:) :: (a->b->c->d->e->f->g->h->i) -> i -> (a->b->c->d->e->f->g->h->i)
(->>>>>>>>:) = const
infixl 1 ->>>>>>>>:

-- | Forces the result type of a 9-argument function.
(->>>>>>>>>:) :: (a->b->c->d->e->f->g->h->i->j) -> j
              -> (a->b->c->d->e->f->g->h->i->j)
(->>>>>>>>>:) = const
infixl 1 ->>>>>>>>>:

-- | Forces the result type of a 10-argument function.
(->>>>>>>>>>:) :: (a->b->c->d->e->f->g->h->i->j->k) -> k
               -> (a->b->c->d->e->f->g->h->i->j->k)
(->>>>>>>>>>:) = const
infixl 1 ->>>>>>>>>>:

-- | Forces the result type of a 11-argument function.
(->>>>>>>>>>>:) :: (a->b->c->d->e->f->g->h->i->j->k->l) -> l
                -> (a->b->c->d->e->f->g->h->i->j->k->l)
(->>>>>>>>>>>:) = const
infixl 1 ->>>>>>>>>>>:

-- | Forces the result type of a 12-argument function.
(->>>>>>>>>>>>:) :: (a->b->c->d->e->f->g->h->i->j->k->l->m) -> m
                 -> (a->b->c->d->e->f->g->h->i->j->k->l->m)
(->>>>>>>>>>>>:) = const
infixl 1 ->>>>>>>>>>>>:
