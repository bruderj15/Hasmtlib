-- Required for class constraints of form: c (ValueType t) :: Constraint
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Language.Hasmtlib.Equatable where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Boolean
import GHC.Generics
import Numeric.Natural
import Data.Int
import Data.Word
import Data.Void
  
-- | Test two as on equality as SMT-Expression.
--   Usage:
--     x <- var @RealType
--     y <- var @RealType
--     assert $ y === x
class Equatable a where
  (===) :: a -> a -> Expr BoolType
  default (===) :: (Generic a, GEquatable (Rep a)) => a -> a -> Expr BoolType
  a === b = from a ===# from b
  
  (/==) :: a -> a -> Expr BoolType
  x /== y = not' $ x === y
  
  {-# MINIMAL (===) #-}
  
infix 4 ===, /==

instance (KnownSMTRepr t, Eq (ValueType t)) => Equatable (Expr t) where
  (===)   = EQU
  x /== y = not' $ EQU x y

class GEquatable f where
  (===#) :: f a -> f a -> Expr BoolType

instance GEquatable U1 where
  U1 ===# U1 = true

instance GEquatable V1 where
  x ===# y = x `seq` y `seq` error "GEquatable[V1].===#"

instance (GEquatable f, GEquatable g) => GEquatable (f :*: g) where
  (a :*: b) ===# (c :*: d) = (a ===# c) &&& (b ===# d)

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