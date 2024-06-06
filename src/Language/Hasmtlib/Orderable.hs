-- Required for class constraints of form: c (ValueType t) :: Constraint
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}

module Language.Hasmtlib.Orderable where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Iteable
import Language.Hasmtlib.Boolean
import Data.Int
import Data.Word
import Data.Void
import Numeric.Natural
import GHC.Generics
import GHC.TypeNats

-- | Compare two as as SMT-Expression.
--   Usage:
--     x <- var @RealType
--     y <- var @RealType
--     b <- var @BoolType
--     assert $ x >? y
--     assert $ x === min' 42 100
class Equatable a => Orderable a where
  (<=?) :: a -> a -> Expr BoolType
  default (<=?) :: (Generic a, GOrderable (Rep a)) => a -> a -> Expr BoolType
  x <=? y = from x <=?# from y

  (>=?) :: a -> a -> Expr BoolType
  x >=? y = y <=? x
  
  (<?)  :: a -> a -> Expr BoolType
  x <? y = not' $ y <=? x

  (>?)  :: a -> a -> Expr BoolType
  x >? y = not' $ x <=? y

infix 4 <?, <=?, >=?, >?

-- | Minimum of two as SMT-Expression.
min' :: (Orderable a, Iteable (Expr BoolType) a) => a -> a -> a
min' x y = ite (x <=? y) x y

-- | Maximum of two as SMT-Expression.
max' :: (Orderable a, Iteable (Expr BoolType) a) => a -> a -> a
max' x y = ite (y <=? x) x y

instance Orderable (Expr IntType) where
  (<?)     = LTH
  (<=?)    = LTHE
  (>=?)    = GTHE
  (>?)     = GTH

instance Orderable (Expr RealType) where
  (<?)     = LTH
  (<=?)    = LTHE
  (>=?)    = GTHE
  (>?)     = GTH

instance KnownNat n => Orderable (Expr (BvType n)) where
  (<?)     = BvuLT
  (<=?)    = BvuLTHE
  (>=?)    = BvuGTHE
  (>?)     = BvuGT

class GEquatable f => GOrderable f where
  (<?#)  :: f a -> f a -> Expr BoolType
  (<=?#) :: f a -> f a -> Expr BoolType

instance GOrderable U1 where
  U1 <?#  U1 = false
  U1 <=?# U1 = true

instance GOrderable V1 where
  x <?# y = x `seq` y `seq` error "GOrderable[V1].<?#"
  x <=?# y = x `seq` y `seq` error "GOrderable[V1].<=?#"

instance (GOrderable f, GOrderable g) => GOrderable (f :*: g) where
  (a :*: b) <?#  (c :*: d) = (a <?# c) ||| (a ===# c &&& b <?# d)
  (a :*: b) <=?# (c :*: d) = (a <?# c) ||| (a ===# c &&& b <=?# d)

instance (GOrderable f, GOrderable g) => GOrderable (f :+: g) where
  L1 _ <?# R1 _ = true
  L1 a <?# L1 b = a <?# b
  R1 a <?# R1 b = a <?# b
  R1 _ <?# L1 _ = false

  L1 _ <=?# R1 _ = true
  L1 a <=?# L1 b = a <=?# b
  R1 a <=?# R1 b = a <=?# b
  R1 _ <=?# L1 _ = false

instance GOrderable f => GOrderable (M1 i c f) where
  M1 x <?#  M1 y = x <?#  y
  M1 x <=?# M1 y = x <=?# y

instance Orderable a => GOrderable (K1 i a) where
  K1 a <?#  K1 b = a <?  b
  K1 a <=?# K1 b = a <=? b

-- Boring instances that end up being useful when deriving Orderable with Generics

instance Orderable ()       where _ <?  _ = false
                                  _ <=? _ = true
instance Orderable Void     where x <?  y = x `seq` y `seq` error "Orderable[Void].<?"
                                  x <=? y = x `seq` y `seq` error "Orderable[Void].<=?"
instance Orderable Int      where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Integer  where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Natural  where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Word     where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Word8    where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Word16   where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Word32   where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Word64   where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Int8     where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Int16    where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Int32    where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Int64    where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Char     where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Float    where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Double   where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Ordering where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)
instance Orderable Bool     where x <?  y = bool (x <  y)
                                  x <=? y = bool (x <= y)