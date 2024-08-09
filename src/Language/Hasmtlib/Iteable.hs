{-# LANGUAGE DefaultSignatures #-}

module Language.Hasmtlib.Iteable where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.SMTSort
import Data.Sequence (Seq)
import Data.Tree
import Data.Monoid (Sum, Product, First, Last, Dual)
import Data.Functor.Identity (Identity)

-- | If condition (p :: b) then (t :: a) else (f :: a)
--
--    >>> ite true "1" "2"
--        "1"
--    >>> ite false 100 42
--        42
class Iteable b a where
  ite :: b -> a -> a -> a
  default ite :: (Iteable b c, Applicative f, f c ~ a) => b -> a -> a -> a
  ite p t f = liftA2 (ite p) t f

instance Iteable (Expr BoolSort) (Expr t) where
  ite = Ite
  {-# INLINE ite #-}

instance Iteable Bool a where
  ite p t f = if p then t else f
  {-# INLINE ite #-}

instance Iteable (Expr BoolSort) a => Iteable (Expr BoolSort) [a]
instance Iteable (Expr BoolSort) a => Iteable (Expr BoolSort) (Maybe a)
instance Iteable (Expr BoolSort) a => Iteable (Expr BoolSort) (Seq a)
instance Iteable (Expr BoolSort) a => Iteable (Expr BoolSort) (Tree a)
instance Iteable (Expr BoolSort) a => Iteable (Expr BoolSort) (Sum a)
instance Iteable (Expr BoolSort) a => Iteable (Expr BoolSort) (Product a)
instance Iteable (Expr BoolSort) a => Iteable (Expr BoolSort) (First a)
instance Iteable (Expr BoolSort) a => Iteable (Expr BoolSort) (Last a)
instance Iteable (Expr BoolSort) a => Iteable (Expr BoolSort) (Dual a)
instance Iteable (Expr BoolSort) a => Iteable (Expr BoolSort) (Identity a)

instance Iteable (Expr BoolSort) () where
  ite _ _ _ = ()

instance (Iteable (Expr BoolSort) a, Iteable (Expr BoolSort) b) => Iteable (Expr BoolSort) (a,b) where
  ite p (a,b) (a',b') = (ite p a a', ite p b b')

instance (Iteable (Expr BoolSort) a, Iteable (Expr BoolSort) b, Iteable (Expr BoolSort) c) => Iteable (Expr BoolSort) (a,b,c) where
  ite p (a,b,c) (a',b',c') = (ite p a a', ite p b b', ite p c c')

instance (Iteable (Expr BoolSort) a, Iteable (Expr BoolSort) b, Iteable (Expr BoolSort) c, Iteable (Expr BoolSort) d) => Iteable (Expr BoolSort) (a,b,c,d) where
  ite p (a,b,c,d) (a',b',c',d') = (ite p a a', ite p b b', ite p c c', ite p d d')

instance (Iteable (Expr BoolSort) a, Iteable (Expr BoolSort) b, Iteable (Expr BoolSort) c, Iteable (Expr BoolSort) d, Iteable (Expr BoolSort) e) => Iteable (Expr BoolSort) (a,b,c,d,e) where
  ite p (a,b,c,d,e) (a',b',c',d',e') = (ite p a a', ite p b b', ite p c c', ite p d d', ite p e e')

instance (Iteable (Expr BoolSort) a, Iteable (Expr BoolSort) b, Iteable (Expr BoolSort) c, Iteable (Expr BoolSort) d, Iteable (Expr BoolSort) e, Iteable (Expr BoolSort) f) => Iteable (Expr BoolSort) (a,b,c,d,e,f) where
  ite p (a,b,c,d,e,f) (a',b',c',d',e',f') = (ite p a a', ite p b b', ite p c c', ite p d d', ite p e e', ite p f f')

instance (Iteable (Expr BoolSort) a, Iteable (Expr BoolSort) b, Iteable (Expr BoolSort) c, Iteable (Expr BoolSort) d, Iteable (Expr BoolSort) e, Iteable (Expr BoolSort) f, Iteable (Expr BoolSort) g) => Iteable (Expr BoolSort) (a,b,c,d,e,f,g) where
  ite p (a,b,c,d,e,f,g) (a',b',c',d',e',f',g') = (ite p a a', ite p b b', ite p c c', ite p d d', ite p e e', ite p f f', ite p g g')

instance (Iteable (Expr BoolSort) a, Iteable (Expr BoolSort) b, Iteable (Expr BoolSort) c, Iteable (Expr BoolSort) d, Iteable (Expr BoolSort) e, Iteable (Expr BoolSort) f, Iteable (Expr BoolSort) g, Iteable (Expr BoolSort) h) => Iteable (Expr BoolSort) (a,b,c,d,e,f,g,h) where
  ite p (a,b,c,d,e,f,g,h) (a',b',c',d',e',f',g',h') = (ite p a a', ite p b b', ite p c c', ite p d d', ite p e e', ite p f f', ite p g g', ite p h h')
