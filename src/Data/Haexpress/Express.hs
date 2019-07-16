-- |
-- Module      : Data.Haexpress.Express
-- Copyright   : (c) 2019 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- Defines the 'Express' type class.
module Data.Haexpress.Express (Express (..)) where

import Data.Haexpress.Core
import Data.Typeable

class Typeable a => Express a where
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
  expr xs@[]      =  value "[]" ([] -: xs)
  expr xs@(y:ys)  =  value ":"  ((:) ->>: xs) :$ expr y :$ expr ys

(-:) :: a -> a -> a
(-:) = asTypeOf -- const
infixl 1 -:

(-:>) :: (a -> b) -> a -> (a -> b)
(-:>) = const
infixl 1 -:>

(->:) :: (a -> b) -> b -> (a -> b)
(->:) = const
infixl 1 ->:

(->:>) :: (a -> b -> c) -> b -> (a -> b -> c)
(->:>) = const
infixl 1 ->:>

(->>:) :: (a -> b -> c) -> c -> (a -> b -> c)
(->>:) = const
infixl 1 ->>:

(->>:>) :: (a -> b -> c -> d) -> c -> (a -> b -> c -> d)
(->>:>) = const
infixl 1 ->>:>

(->>>:) :: (a -> b -> c -> d) -> d -> (a -> b -> c -> d)
(->>>:) = const
infixl 1 ->>>:

(->>>:>) :: (a -> b -> c -> d -> e) -> d -> (a -> b -> c -> d -> e)
(->>>:>) = const
infixl 1 ->>>:>

(->>>>:) :: (a -> b -> c -> d -> e) -> e -> (a -> b -> c -> d -> e)
(->>>>:) = const
infixl 1 ->>>>:

(->>>>:>) :: (a -> b -> c -> d -> e -> f) -> e -> (a -> b -> c -> d -> e -> f)
(->>>>:>) = const
infixl 1 ->>>>:>

(->>>>>:) :: (a -> b -> c -> d -> e -> f) -> f -> (a -> b -> c -> d -> e -> f)
(->>>>>:) = const
infixl 1 ->>>>>:

(->>>>>:>) :: (a->b->c->d->e->f->g) -> f -> (a->b->c->d->e->f->g)
(->>>>>:>) = const
infixl 1 ->>>>>:>

(->>>>>>:) :: (a->b->c->d->e->f->g) -> g -> (a->b->c->d->e->f->g)
(->>>>>>:) = const
infixl 1 ->>>>>>:
