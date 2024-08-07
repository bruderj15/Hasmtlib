-- Required for class constraints of form: c (ValueType t) :: Constraint
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Language.Hasmtlib.Equatable where

import Prelude hiding (not, (&&))
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Boolean
import Data.Int
import Data.Word
import Data.Void
import Data.Tree (Tree)
import Data.Monoid (Sum, Product, First, Last, Dual)
import Data.Functor.Identity (Identity)
import qualified Data.Vector.Sized as V
import Numeric.Natural
import GHC.Generics

-- | Test two as on equality as SMT-Expression.
--
--   You can derive an instance of this class if your type is 'Generic'.
--
-- @
--     x <- var @RealType
--     y <- var
--     assert $ y === x && not (y /== x)
-- @
--
class Equatable a where
  -- | Test whether two values are equal in the SMT-Problem.
  (===) :: a -> a -> Expr BoolSort
  default (===) :: (Generic a, GEquatable (Rep a)) => a -> a -> Expr BoolSort
  a === b = from a ===# from b

  -- | Test whether two values are not equal in the SMT-Problem.
  (/==) :: a -> a -> Expr BoolSort
  x /== y = not $ x === y

infix 4 ===, /==

instance (KnownSMTSort t, Eq (HaskellType t)) => Equatable (Expr t) where
  x === y = EQU $ V.fromTuple (x,y)
  {-# INLINE (===) #-}
  x /== y = Distinct $ V.fromTuple (x,y)
  {-# INLINE (/==) #-}

class GEquatable f where
  (===#) :: f a -> f a -> Expr BoolSort

instance GEquatable U1 where
  U1 ===# U1 = true

instance GEquatable V1 where
  x ===# y = x `seq` y `seq` error "GEquatable[V1].===#"

instance (GEquatable f, GEquatable g) => GEquatable (f :*: g) where
  (a :*: b) ===# (c :*: d) = (a ===# c) && (b ===# d)

instance (GEquatable f, GEquatable g) => GEquatable (f :+: g) where
  L1 a ===# L1 b = a ===# b
  R1 a ===# R1 b = a ===# b
  _ ===# _ = false

instance GEquatable f => GEquatable (M1 i c f) where
  M1 x ===# M1 y = x ===# y

instance Equatable a => GEquatable (K1 i a) where
  K1 a ===# K1 b = a === b

instance Equatable ()       where _ === _ = true
instance Equatable Void     where x === y = x `seq` y `seq` error "Equatable[Void].==="
instance Equatable Int      where x === y = bool (x == y)
instance Equatable Integer  where x === y = bool (x == y)
instance Equatable Natural  where x === y = bool (x == y)
instance Equatable Word     where x === y = bool (x == y)
instance Equatable Word8    where x === y = bool (x == y)
instance Equatable Word16   where x === y = bool (x == y)
instance Equatable Word32   where x === y = bool (x == y)
instance Equatable Word64   where x === y = bool (x == y)
instance Equatable Int8     where x === y = bool (x == y)
instance Equatable Int16    where x === y = bool (x == y)
instance Equatable Int32    where x === y = bool (x == y)
instance Equatable Int64    where x === y = bool (x == y)
instance Equatable Char     where x === y = bool (x == y)
instance Equatable Float    where x === y = bool (x == y)
instance Equatable Double   where x === y = bool (x == y)
instance Equatable Ordering where x === y = bool (x == y)
instance Equatable Bool     where x === y = bool (x == y)
instance (Equatable a, Equatable b) => Equatable (a,b)
instance (Equatable a, Equatable b, Equatable c) => Equatable (a,b,c)
instance (Equatable a, Equatable b, Equatable c, Equatable d) => Equatable (a,b,c,d)
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e) => Equatable (a,b,c,d,e)
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f) => Equatable (a,b,c,d,e,f)
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f, Equatable g) => Equatable (a,b,c,d,e,f,g)
instance (Equatable a, Equatable b, Equatable c, Equatable d, Equatable e, Equatable f, Equatable g, Equatable h) => Equatable (a,b,c,d,e,f,g,h)
instance Equatable a => Equatable [a]
instance Equatable a => Equatable (Tree a)
instance Equatable a => Equatable (Maybe a)
instance (Equatable a, Equatable b) => Equatable (Either a b)
instance Equatable a => Equatable (Sum a)
instance Equatable a => Equatable (Product a)
instance Equatable a => Equatable (First a)
instance Equatable a => Equatable (Last a)
instance Equatable a => Equatable (Dual a)
instance Equatable a => Equatable (Identity a)
