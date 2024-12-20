{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}

{- |
This module provides the data-type 'Expr'.

It represents SMTLib-expressions via an abstract syntax tree (AST), implemented as GADT.

Variables are just 'Int's wrapped in a newtype 'SMTVar' with a phantom-type 'SMTSort'.

Usually the end user of this library does not need to deal with this representation.
Instead he should rely on the provided instances for building expressions.
Some of the main classes of these include:

1. 'Equatable' and 'Orderable' for symbolic comparisons,

2. 'Iteable' for symbolic branching via 'ite',

3. 'Boolean' for symbolic bool operations,

4. Prelude classics like: 'Num', 'Floating', 'Integral', 'Bounded', ... for arithmetics

5. 'Bits.Bits' for BitVec-operations

Besides that, there are also some operations defined by the SMTLib-Standard Version 2.6 that do not fit into any classes
and therefore are exported as plain functions, like 'for_all' or 'bvConcat'.
-}
module Language.Hasmtlib.Type.Expr
  (
  -- * SMTVar
    SMTVar(..), varId

  -- * Expr type
  , Expr(..), isLeaf, exprSize

  -- * Compare
  -- ** Equatable
  -- *** Class
  , Equatable(..)
  , equal, distinct

  -- *** Generic
  , GEquatable(..)

  -- ** Orderable
  -- *** Class
  , Orderable(..)
  , min', max'

  -- *** Generic
  , GOrderable(..)

  -- ** Iteable
  , Iteable(..)

  -- * Non-class functions
  -- ** Quantifier
  , for_all, exists

  -- ** BitVec
  , bvConcat

  -- ** Array
  , select, store

  -- ** String
  , strLength, strAt, strSubstring, strPrefixOf, strSuffixOf, strContains, strIndexOf, strReplace, strReplaceAll

  -- ** Conversion
  , toRealSort, toIntSort, isIntSort
  )
where

import Prelude hiding (not, and, or, any, all, (&&), (||))
import Language.Hasmtlib.Internal.Uniplate1
import Language.Hasmtlib.Type.Bitvec (BvEnc(..), KnownBvEnc(..), SBvEnc(..))
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Value
import Language.Hasmtlib.Boolean
import Data.GADT.Compare
import Data.GADT.DeepSeq
import Data.Coerce
import Data.Proxy
import Data.Int
import Data.Word
import Data.Void
import qualified Data.Bits as Bits
import Data.Sequence (Seq)
import Data.Tree (Tree)
import Data.STRef
import Data.Monoid (Sum, Product, First, Last, Dual)
import Data.String (IsString(..))
import Data.Text (pack)
import Data.List(genericLength)
import Data.Foldable (toList)
import qualified Data.Vector.Sized as V
import Control.Lens hiding (from, to)
import Control.Monad.ST
import Control.Monad
import GHC.TypeLits hiding (someNatVal)
import GHC.TypeNats (someNatVal)
import GHC.Generics

-- | An internal SMT variable with a phantom-type which holds an 'Int' as it's identifier.
type role SMTVar phantom
newtype SMTVar (t :: SMTSort) = SMTVar { _varId :: Int }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)
$(makeLenses ''SMTVar)

-- | An SMT-Expression.
--   For building expressions use the corresponding instances.
--
--   With a lot of criminal energy you may build invalid expressions regarding the SMTLib Version 2.6 - specification.
--   Therefore it is highly recommended to rely on the instances.
data Expr (t :: SMTSort) where
  Var       :: KnownSMTSort t => SMTVar t -> Expr t
  Constant  :: Value t -> Expr t
  Plus      :: Num (HaskellType t) => Expr t -> Expr t -> Expr t
  Minus     :: Num (HaskellType t) => Expr t -> Expr t -> Expr t
  Neg       :: Num (HaskellType t) => Expr t -> Expr t
  Mul       :: Num (HaskellType t) => Expr t -> Expr t -> Expr t
  Abs       :: Num (HaskellType t) => Expr t -> Expr t
  Mod       :: Integral (HaskellType t) => Expr t -> Expr t  -> Expr t
  Rem       :: Integral (HaskellType t) => Expr t -> Expr t  -> Expr t
  IDiv      :: Integral (HaskellType t) => Expr t -> Expr t  -> Expr t
  Div       :: Expr RealSort -> Expr RealSort -> Expr RealSort
  LTH       :: (Ord (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort
  LTHE      :: (Ord (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort
  EQU       :: (Eq (HaskellType t), KnownSMTSort t, KnownNat n) => V.Vector (n + 2) (Expr t) -> Expr BoolSort
  Distinct  :: (Eq (HaskellType t), KnownSMTSort t, KnownNat n) => V.Vector (n + 2) (Expr t) -> Expr BoolSort
  GTHE      :: (Ord (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort
  GTH       :: (Ord (HaskellType t), KnownSMTSort t) => Expr t -> Expr t -> Expr BoolSort
  Not       :: Boolean (HaskellType t) => Expr t -> Expr t
  And       :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t
  Or        :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t
  Impl      :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t
  Xor       :: Boolean (HaskellType t) => Expr t -> Expr t -> Expr t
  Pi        :: Expr RealSort
  Sqrt      :: Expr RealSort -> Expr RealSort
  Exp       :: Expr RealSort -> Expr RealSort
  Sin       :: Expr RealSort -> Expr RealSort
  Cos       :: Expr RealSort -> Expr RealSort
  Tan       :: Expr RealSort -> Expr RealSort
  Asin      :: Expr RealSort -> Expr RealSort
  Acos      :: Expr RealSort -> Expr RealSort
  Atan      :: Expr RealSort -> Expr RealSort
  ToReal    :: Expr IntSort  -> Expr RealSort
  ToInt     :: Expr RealSort -> Expr IntSort
  IsInt     :: Expr RealSort -> Expr BoolSort
  Ite       :: Expr BoolSort -> Expr t -> Expr t -> Expr t
  BvNand    :: (KnownBvEnc enc, KnownNat n) => Expr (BvSort enc n) -> Expr (BvSort enc n) -> Expr (BvSort enc n)
  BvNor     :: (KnownBvEnc enc, KnownNat n) => Expr (BvSort enc n) -> Expr (BvSort enc n) -> Expr (BvSort enc n)
  BvShL     :: (KnownBvEnc enc, KnownNat n) => Expr (BvSort enc n) -> Expr (BvSort enc n) -> Expr (BvSort enc n)
  BvLShR    :: KnownNat n => Expr (BvSort Unsigned n) -> Expr (BvSort Unsigned n) -> Expr (BvSort Unsigned n)
  BvAShR    :: KnownNat n => Expr (BvSort Signed n) -> Expr (BvSort Signed n) -> Expr (BvSort Signed n)
  BvConcat  :: (KnownBvEnc enc , KnownNat n, KnownNat m) => Expr (BvSort enc n) -> Expr (BvSort enc m) -> Expr (BvSort enc (n + m))
  BvRotL    :: (KnownBvEnc enc, KnownNat n, Integral a) => a -> Expr (BvSort enc n) -> Expr (BvSort enc n)
  BvRotR    :: (KnownBvEnc enc, KnownNat n, Integral a) => a -> Expr (BvSort enc n) -> Expr (BvSort enc n)
  ArrSelect :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Ord (HaskellType v)) => Expr (ArraySort k v) -> Expr k -> Expr v
  ArrStore  :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v -> Expr (ArraySort k v)
  StrConcat     :: Expr StringSort -> Expr StringSort -> Expr StringSort
  StrLength     :: Expr StringSort -> Expr IntSort
  StrAt         :: Expr StringSort -> Expr IntSort -> Expr StringSort
  StrSubstring  :: Expr StringSort -> Expr IntSort -> Expr IntSort -> Expr StringSort
  StrPrefixOf   :: Expr StringSort -> Expr StringSort -> Expr BoolSort
  StrSuffixOf   :: Expr StringSort -> Expr StringSort -> Expr BoolSort
  StrContains   :: Expr StringSort -> Expr StringSort -> Expr BoolSort
  StrIndexOf    :: Expr StringSort -> Expr StringSort -> Expr IntSort -> Expr IntSort
  StrReplace    :: Expr StringSort -> Expr StringSort -> Expr StringSort -> Expr StringSort
  StrReplaceAll :: Expr StringSort -> Expr StringSort -> Expr StringSort -> Expr StringSort
  -- | Just v if quantified var has been created already, Nothing otherwise
  ForAll    :: KnownSMTSort t => Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Expr BoolSort
  -- | Just v if quantified var has been created already, Nothing otherwise
  Exists    :: KnownSMTSort t => Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Expr BoolSort

-- | Indicates whether an expression is a leaf.
--   All non-recursive contructors are leafs.
isLeaf :: Expr t -> Bool
isLeaf (Var _) = True
isLeaf (Constant _) = True
isLeaf Pi = True
isLeaf _ = False
{-# INLINE isLeaf #-}

-- | Size of the expression.
--
--   Counts the amount of operations.
--
-- ==== __Examples__
--
--    >>> nodeSize $ x + y === x + y
--        3
--    >>> nodeSize $ false
--        0
exprSize :: KnownSMTSort t => Expr t -> Integer
exprSize expr = runST $ do
  nodesRef <- newSTRef 0
  _ <- transformM1
    (\expr' -> do
      unless (isLeaf expr') $ modifySTRef' nodesRef (+1)
      return expr')
    expr
  readSTRef nodesRef

-- | Class that allows branching on predicates of type @b@ on branches of type @a@.
--
--   If predicate (p :: b) then (t :: a) else (f :: a).
--
--   There is a default implementation if your type is an 'Applicative'.
--
-- ==== __Examples__
--
--    >>> ite True "1" "2"
--        "1"
--    >>> ite False 100 42
--        42
class Iteable b a where
  ite :: b -> a -> a -> a
  default ite :: (Iteable b c, Applicative f, f c ~ a) => b -> a -> a -> a
  ite p t f = ite p <$> t <*> f

instance Iteable (Expr BoolSort) (Expr t) where
  ite (Constant (BoolValue False)) _ f = f
  ite (Constant (BoolValue True)) t _  = t
  ite p t@(Ite p' t' f') f@(Ite p'' t'' f'')
    | p' == p'' && t' == t'' = Ite p' t' (Ite p f' f'')
    | p' == p'' && f' == f'' = Ite (not p') f' (Ite p t' t'')
    | otherwise = Ite p t f
  ite p t f@(Ite p' t' f')
    | p == p' = Ite p t f'
    | t == t' = Ite (p || p') t f'
    | otherwise = Ite p t f
  ite p t@(Ite p' t' f') f
    | p == p' = Ite p t' f
    | f == f' = Ite (p && p') t' f
    | otherwise = Ite p t f
  ite p t f
    | t == f = t
    | otherwise = Ite p t f
  {-# INLINEABLE ite #-}

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

-- | Symbolically test two values on equality.
--
-- A generic default implementation with 'GEquatable' is possible.
--
-- ==== __Example__
--
-- @
-- x <- var @RealType
-- y <- var
-- assert $ y === x && not (y /== x) && x === 42
-- @
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

-- | Symbolically compare two values.
--
-- A generic default implementation with 'GOrderable' is possible.
--
-- ==== __Example__
--
-- @
-- x <- var @RealSort
-- y <- var
-- assert $ x >? y
-- assert $ x === min' 42 100
-- @
--
class Equatable a => Orderable a where
  (<=?) :: a -> a -> Expr BoolSort
  default (<=?) :: (Generic a, GOrderable (Rep a)) => a -> a -> Expr BoolSort
  x <=? y = from x <=?# from y

  (>=?) :: a -> a -> Expr BoolSort
  x >=? y = y <=? x

  (<?)  :: a -> a -> Expr BoolSort
  x <? y = not $ y <=? x

  (>?)  :: a -> a -> Expr BoolSort
  x >? y = not $ x <=? y

infix 4 <?, <=?, >=?, >?

-- | Symbolic evaluation of the minimum of two symbolic values.
min' :: (Orderable a, Iteable (Expr BoolSort) a) => a -> a -> a
min' x y = ite (x <=? y) x y

-- | Symbolic evaluation of the maximum of two symbolic values.
max' :: (Orderable a, Iteable (Expr BoolSort) a) => a -> a -> a
max' x y = ite (y <=? x) x y

instance (KnownSMTSort t, Ord (HaskellType t)) => Orderable (Expr t) where
  (<?)     = LTH
  {-# INLINE (<?) #-}
  (<=?)    = LTHE
  {-# INLINE (<=?) #-}
  (>=?)    = GTHE
  {-# INLINE (>=?) #-}
  (>?)     = GTH
  {-# INLINE (>?) #-}

class GEquatable f => GOrderable f where
  (<?#)  :: f a -> f a -> Expr BoolSort
  (<=?#) :: f a -> f a -> Expr BoolSort

instance GOrderable U1 where
  U1 <?#  U1 = false
  U1 <=?# U1 = true

instance GOrderable V1 where
  x <?# y = x `seq` y `seq` error "GOrderable[V1].<?#"
  x <=?# y = x `seq` y `seq` error "GOrderable[V1].<=?#"

instance (GOrderable f, GOrderable g) => GOrderable (f :*: g) where
  (a :*: b) <?#  (c :*: d) = (a <?# c) || (a ===# c && b <?# d)
  (a :*: b) <=?# (c :*: d) = (a <?# c) || (a ===# c && b <=?# d)

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

instance (Orderable a, Orderable b) => Orderable (a,b)
instance (Orderable a, Orderable b, Orderable c) => Orderable (a,b,c)
instance (Orderable a, Orderable b, Orderable c, Orderable d) => Orderable (a,b,c,d)
instance (Orderable a, Orderable b, Orderable c, Orderable d, Orderable e) => Orderable (a,b,c,d,e)
instance (Orderable a, Orderable b, Orderable c, Orderable d, Orderable e, Orderable f) => Orderable (a,b,c,d,e,f)
instance (Orderable a, Orderable b, Orderable c, Orderable d, Orderable e, Orderable f, Orderable g) => Orderable (a,b,c,d,e,f,g)
instance (Orderable a, Orderable b, Orderable c, Orderable d, Orderable e, Orderable f, Orderable g, Orderable h) => Orderable (a,b,c,d,e,f,g,h)
instance Orderable a => Orderable [a]
instance Orderable a => Orderable (Tree a)
instance Orderable a => Orderable (Maybe a)
instance (Orderable a, Orderable b) => Orderable (Either a b)
instance Orderable a => Orderable (Sum a)
instance Orderable a => Orderable (Product a)
instance Orderable a => Orderable (First a)
instance Orderable a => Orderable (Last a)
instance Orderable a => Orderable (Dual a)
instance Orderable a => Orderable (Identity a)

-- | Symbolically test multiple expressions on equality.
--
--   Returns 'true' if given less than two arguments.
equal :: (Eq (HaskellType t), KnownSMTSort t, Foldable f) => f (Expr t) -> Expr BoolSort
equal (toList -> (a:b:xs)) = case someNatVal (genericLength xs) of
  SomeNat n -> case V.fromListN' n xs of
    Nothing  -> EQU $ V.fromTuple (a,b)
    Just xs' -> EQU $ xs' V.++ V.fromTuple (a,b)
equal (toList -> _)        = true

-- | Symbolically test multiple expressions on distinctness.
--
--   Returns 'true' if given less than two arguments.
distinct :: (Eq (HaskellType t), KnownSMTSort t, Foldable f) => f (Expr t) -> Expr BoolSort
distinct (toList -> (a:b:xs)) = case someNatVal (genericLength xs) of
  SomeNat n -> case V.fromListN' n xs of
    Nothing  -> Distinct $ V.fromTuple (a,b)
    Just xs' -> Distinct $ xs' V.++ V.fromTuple (a,b)
distinct (toList -> _)        = true

-- | Universal quantification for any specific 'SMTSort'.
--
-- ==== __Example__
--
--   @
--   assert $
--      for_all @IntSort $ \x ->
--         x + 0 === x && 0 + x === x
--   @
--
--   The lambdas 'x' is all-quantified here.
--   It will only be scoped for the lambdas body.
for_all :: forall t. KnownSMTSort t => (Expr t -> Expr BoolSort) -> Expr BoolSort
for_all = ForAll Nothing
{-# INLINE for_all #-}

-- | Existential quantification for any specific 'SMTSort'
--
-- ==== __Example__
--
--   @
--   assert $
--      for_all @(BvSort Unsigned 8) $ \x ->
--          exists $ \y ->
--            x - y === 0
--   @
--
--   The lambdas 'y' is existentially quantified here.
--   It will only be scoped for the lambdas body.
exists :: forall t. KnownSMTSort t => (Expr t -> Expr BoolSort) -> Expr BoolSort
exists = Exists Nothing
{-# INLINE exists #-}

-- | Select a value from an array.
select :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Ord (HaskellType v)) => Expr (ArraySort k v) -> Expr k -> Expr v
select = ArrSelect
{-# INLINE select #-}

-- | Store a value in an array.
store :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v -> Expr (ArraySort k v)
store = ArrStore
{-# INLINE store #-}

-- | Concats two bitvectors.
bvConcat :: (KnownBvEnc enc, KnownNat n, KnownNat m) => Expr (BvSort enc n) -> Expr (BvSort enc m) -> Expr (BvSort enc (n + m))
bvConcat = BvConcat
{-# INLINE bvConcat #-}

-- | Converts an expression of type 'IntSort' to type 'RealSort'.
toRealSort :: Expr IntSort  -> Expr RealSort
toRealSort = ToReal
{-# INLINE toRealSort #-}

-- | Converts an expression of type 'RealSort' to type 'IntSort'.
toIntSort :: Expr RealSort -> Expr IntSort
toIntSort = ToInt
{-# INLINE toIntSort #-}

-- | Checks whether an expression of type 'RealSort' may be safely converted to type 'IntSort'.
isIntSort :: Expr RealSort -> Expr BoolSort
isIntSort = IsInt
{-# INLINE isIntSort #-}

-- | Length of a string.
strLength :: Expr StringSort -> Expr IntSort
strLength = StrLength
{-# INLINE strLength #-}

-- | Singleton string containing a character at given position
--   or empty string when position is out of range.
--   The leftmost position is 0.
strAt :: Expr StringSort -> Expr IntSort -> Expr StringSort
strAt = StrAt
{-# INLINE strAt #-}

-- | @(strSubstring s i n)@ evaluates to the longest (unscattered) substring
--   of @s@ of length at most @n@ starting at position @i@.
--   It evaluates to the empty string if @n@ is negative or @i@ is not in
--   the interval @[0,l-1]@ where @l@ is the length of @s@.
strSubstring :: Expr StringSort -> Expr IntSort -> Expr IntSort -> Expr StringSort
strSubstring = StrSubstring
{-# INLINE strSubstring #-}

-- | First string is a prefix of second one.
--   @(strPrefixof s t)@ is @true@ iff @s@ is a prefix of @t@.
strPrefixOf :: Expr StringSort -> Expr StringSort -> Expr BoolSort
strPrefixOf = StrPrefixOf
{-# INLINE strPrefixOf #-}

-- | First string is a suffix of second one.
--   @(strSuffixof s t)@ is @true@ iff @s@ is a suffix of @t@.
strSuffixOf :: Expr StringSort -> Expr StringSort -> Expr BoolSort
strSuffixOf = StrSuffixOf
{-# INLINE strSuffixOf #-}

-- | First string contains second one
--   @(strContains s t)@ iff @s@ contains @t@.
strContains :: Expr StringSort -> Expr StringSort -> Expr BoolSort
strContains = StrContains
{-# INLINE strContains #-}

-- | Index of first occurrence of second string in first one starting at the position specified by the third argument.
--   @(strIndexof s t i)@, with @0 <= i <= |s|@ is the position of the first
--   occurrence of @t@ in @s@ at or after position @i@, if any.
--   Otherwise, it is @-1@. Note that the result is @i@ whenever @i@ is within
--   the range @[0, |s|]@ and @t@ is empty.
strIndexOf :: Expr StringSort -> Expr StringSort -> Expr IntSort -> Expr IntSort
strIndexOf = StrIndexOf
{-# INLINE strIndexOf #-}

-- | @(strReplace s t t')@ is the string obtained by replacing the first
--   occurrence of @t@ in @s@, if any, by @t'@. Note that if @t@ is empty, the
--   result is to prepend @t'@ to @s@; also, if @t@ does not occur in @s@ then
--   the result is @s@.
strReplace :: Expr StringSort -> Expr StringSort -> Expr StringSort -> Expr StringSort
strReplace = StrReplace
{-# INLINE strReplace #-}

-- | @(strReplaceAll s t t’)@ is @s@ if @t@ is the empty string. Otherwise, it
--   is the string obtained from @s@ by replacing all occurrences of @t@ in @s@
--   by @t’@, starting with the first occurrence and proceeding in left-to-right order.
strReplaceAll :: Expr StringSort -> Expr StringSort -> Expr StringSort -> Expr StringSort
strReplaceAll = StrReplaceAll
{-# INLINE strReplaceAll #-}

instance (KnownSMTSort t, Num (HaskellType t), Ord (HaskellType t)) => Num (Expr t) where
   fromInteger = Constant . wrapValue . fromInteger
   {-# INLINE fromInteger #-}
   (Constant 0) + y = y
   x + (Constant 0) = x
   (Constant x) + (Constant y) = Constant (x + y)
   x + y = Plus x y
   {-# INLINE (+) #-}
   x - (Constant 0) = x
   (Constant x) - (Constant y) = Constant (x - y)
   (Constant 0) - x = negate x
   x - y = Minus x y
   {-# INLINE (-) #-}
   (Constant 0) * _ = 0
   _ * (Constant 0) = 0
   (Constant 1) * y = y
   x * (Constant 1) = x
   (Constant (-1)) * x = negate x
   x * (Constant (-1)) = negate x
   (Constant x) * (Constant y) = Constant (x * y)
   x * y = Mul x y
   {-# INLINE (*) #-}
   negate (Constant x) = Constant $ negate x
   negate (Neg x)      = x
   negate x            = Neg x
   {-# INLINE negate #-}
   abs (Constant x) = Constant $ abs x
   abs x            = Abs x
   {-# INLINE abs #-}
   signum (Constant x) = Constant $ signum x
   signum x            = ite (x === 0) 0 $ ite (x <? 0) (-1) 1
   {-# INLINE signum #-}

instance Fractional (Expr RealSort) where
  fromRational = Constant . RealValue . fromRational
  {-# INLINE fromRational #-}
  x / (Constant  1) = x
  (Constant 0) / _ = 0
  (Constant x) / (Constant y) = Constant (x / y)
  x / y          = Div x y
  {-# INLINE (/) #-}

-- | Not part of the SMTLib standard Version 2.6.
--   Some solvers support it. At least valid for CVC5 and MathSAT.
instance Floating (Expr RealSort) where
    pi    = Pi
    {-# INLINE pi #-}
    exp   = Exp
    {-# INLINE exp #-}
    log   = error "SMT-Solvers currently do not support log"
    sqrt  = Sqrt
    {-# INLINE sqrt #-}
    sin   = Sin
    {-# INLINE sin #-}
    cos   = Cos
    {-# INLINE cos #-}
    tan   = Tan
    {-# INLINE tan #-}
    asin  = Asin
    {-# INLINE asin #-}
    acos  = Acos
    {-# INLINE acos #-}
    atan  = Atan
    {-# INLINE atan #-}
    sinh  = error "SMT-Solvers currently do not support sinh"
    cosh  = error "SMT-Solvers currently do not support cosh"
    tanh  = error "SMT-Solvers currently do not support tanh"
    asinh = error "SMT-Solvers currently do not support asinh"
    acosh = error "SMT-Solvers currently do not support acosh"
    atanh = error "SMT-Solvers currently do not support atanh"

-- | This instance is __partial__ for 'toRational', this method is only intended for use with constants.
instance (KnownSMTSort t, Real (HaskellType t)) => Real (Expr t) where
  toRational (Constant x) = toRational $ unwrapValue x
  toRational _ = error $ "Real#toRational[Expr " <> show (sortSing @t) <> "] only supported for constants."
  {-# INLINE toRational #-}

-- | This instance is __partial__ for 'fromEnum', this method is only intended for use with constants.
instance (KnownSMTSort t, Enum (HaskellType t)) => Enum (Expr t) where
  fromEnum (Constant x) = fromEnum $ unwrapValue x
  fromEnum _ = error $ "Enum#fromEnum[Expr " <> show (sortSing @t) <> "] only supported for constants."
  {-# INLINE fromEnum #-}
  toEnum = Constant . wrapValue . toEnum
  {-# INLINE toEnum #-}

-- | This instance is __partial__ for 'toInteger', this method is only intended for use with constants.
instance (KnownSMTSort t, Integral (HaskellType t)) => Integral (Expr t) where
  quotRem x y = (IDiv x y, Rem x y)
  {-# INLINE quotRem #-}
  divMod x y  = (IDiv x y, Mod x y)
  {-# INLINE divMod #-}
  toInteger (Constant x) = toInteger $ unwrapValue x
  toInteger _ = error $ "Integer#toInteger[Expr " <> show (sortSing @t) <> "] only supported for constants."
  {-# INLINE toInteger #-}

instance Boolean (Expr BoolSort) where
  bool = Constant . BoolValue
  {-# INLINE bool #-}
  (Constant (BoolValue x)) && y = if x then y else false
  x && (Constant (BoolValue y)) = if y then x else false
  x && y = And x y
  {-# INLINE (&&) #-}
  (Constant (BoolValue x)) || y = if x then true else y
  x || (Constant (BoolValue y)) = if y then true else x
  x || y = Or x y
  {-# INLINE (||) #-}
  not (Constant x) = Constant $ not x
  not (Not x) = x
  not x = Not x
  {-# INLINE not #-}
  xor (Constant (BoolValue x)) y  = if x then not y else y
  xor x (Constant (BoolValue y)) = if y then not x else x
  xor x y = Xor x y
  {-# INLINE xor #-}
  (Constant (BoolValue False)) ==> _ = true
  x ==> y  = Impl x y
  {-# INLINE (==>) #-}
  (<==>) = (===)
  {-# INLINE (<==>) #-}

instance (KnownBvEnc enc, KnownNat n) => Boolean (Expr (BvSort enc n)) where
  bool = Constant . BvValue . bool
  {-# INLINE bool #-}
  (&&) = And
  {-# INLINE (&&) #-}
  (||) = Or
  {-# INLINE (||) #-}
  not (Not x) = x
  not x = Not x
  {-# INLINE not #-}
  xor  = Xor
  {-# INLINE xor #-}

instance Bounded (Expr BoolSort) where
  minBound = false
  {-# INLINE minBound #-}
  maxBound = true
  {-# INLINE maxBound #-}

instance (KnownBvEnc enc, KnownNat n) => Bounded (Expr (BvSort enc n)) where
  minBound = Constant $ BvValue minBound
  {-# INLINE minBound #-}
  maxBound = Constant $ BvValue maxBound
  {-# INLINE maxBound #-}

-- | This instance is __partial__ for 'testBit' and 'popCount', it's only intended for use with constants ('Constant').
instance Bits.Bits (Expr BoolSort) where
  (.&.) = And
  {-# INLINE (.&.) #-}
  (.|.) = Or
  {-# INLINE (.|.) #-}
  xor = Xor
  {-# INLINE xor #-}
  complement = Not
  {-# INLINE complement #-}
  zeroBits = false
  {-# INLINE zeroBits #-}
  bit _ = true
  {-# INLINE bit #-}
  setBit _ _ = true
  {-# INLINE setBit #-}
  clearBit _ _ = false
  {-# INLINE clearBit #-}
  complementBit b _ = Not b
  {-# INLINE complementBit #-}
  testBit (Constant (BoolValue b)) _ = b
  testBit _ _ = error "Bits#testBit[Expr BoolSort] is only supported for constants."
  {-# INLINE testBit #-}
  bitSizeMaybe _ = Just 1
  {-# INLINE bitSizeMaybe #-}
  bitSize _ = 1
  {-# INLINE bitSize #-}
  isSigned _ = False
  {-# INLINE isSigned #-}
  shiftL b 0 = b
  shiftL _ _ = false
  {-# INLINE shiftL #-}
  shiftR b 0 = b
  shiftR _ _ = false
  {-# INLINE shiftR #-}
  rotateL b _ = b
  {-# INLINE rotateL #-}
  rotateR b _ = b
  {-# INLINE rotateR #-}
  popCount (Constant (BoolValue b)) = if b then 1 else 0
  popCount _ = error "Bits#popCount[Expr BoolSort] is only supported for constants."
  {-# INLINE popCount #-}

-- | This instance is __partial__ for 'testBit' and 'popCount', it's only intended for use with constants ('Constant').
instance (KnownBvEnc enc, KnownNat n) => Bits.Bits (Expr (BvSort enc n)) where
  (.&.) = And
  {-# INLINE (.&.) #-}
  (.|.) = Or
  {-# INLINE (.|.) #-}
  xor = Xor
  {-# INLINE xor #-}
  complement = Not
  {-# INLINE complement #-}
  zeroBits = false
  {-# INLINE zeroBits #-}
  bit = Constant . BvValue . Bits.bit
  {-# INLINE bit #-}
  testBit (Constant (BvValue b)) i = Bits.testBit b i
  testBit _ _ = error "Bits#testBit[Expr BvSort] is only supported for constants."
  {-# INLINE testBit #-}
  bitSizeMaybe _ = Just $ fromIntegral $ natVal $ Proxy @n
  {-# INLINE bitSizeMaybe #-}
  bitSize _ = fromIntegral $ natVal $ Proxy @n
  {-# INLINE bitSize #-}
  isSigned _ = case bvEncSing @enc of
    SUnsigned -> False
    SSigned   -> True
  {-# INLINE isSigned #-}
  shiftL b i = BvShL b (fromIntegral i)
  {-# INLINE shiftL #-}
  shiftR b i = case bvEncSing @enc of
    SUnsigned -> BvLShR b (fromIntegral i)
    SSigned   -> BvAShR b (fromIntegral i)
  {-# INLINE shiftR #-}
  rotateL b i = BvRotL i b
  {-# INLINE rotateL #-}
  rotateR b i = BvRotR i b
  {-# INLINE rotateR #-}
  popCount (Constant (BvValue b)) = Bits.popCount b
  popCount _ = error $ "Bits#popCount[Expr BvSort] is only supported for constants."
  {-# INLINE popCount #-}

instance Semigroup (Expr StringSort) where
  (<>) = StrConcat
  {-# INLINE (<>) #-}

instance Monoid (Expr StringSort) where
  mempty = Constant $ StringValue mempty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance IsString (Expr StringSort) where
  fromString = Constant . StringValue . pack
  {-# INLINE fromString #-}

type instance Index   (Expr StringSort) = Expr IntSort
type instance IxValue (Expr StringSort) = Expr StringSort

instance Ixed (Expr StringSort) where
  ix i f s = f (strAt s i) <&> \a ->
    let l = strSubstring a 0 i
        r = strSubstring a i (strLength a)
     in l <> strReplace r (strAt a i) s

instance AsEmpty (Expr StringSort) where
  _Empty = prism'
    (const mempty)
    (\s -> ite @(Expr BoolSort) (s === mempty) (Just ()) Nothing)

instance Prefixed (Expr StringSort) where
  prefixed p = prism'
    (p <>)
    (\s -> ite (p `strPrefixOf` s) (Just $ strReplace s p mempty) Nothing)

instance Suffixed (Expr StringSort) where
  suffixed qs = prism'
    (<> qs)
    (\s -> ite (qs `strSuffixOf` s) (Just $ strSubstring s 0 (strLength s - strLength qs)) Nothing)

instance Cons (Expr StringSort) (Expr StringSort) (Expr StringSort) (Expr StringSort) where
  _Cons = prism'
    (uncurry (<>))
    (\s -> ite @(Expr BoolSort) (strLength s >? 0) (Just (strAt s 0, strSubstring s 1 (strLength s))) Nothing)

instance Snoc (Expr StringSort) (Expr StringSort) (Expr StringSort) (Expr StringSort) where
  _Snoc = prism'
    (uncurry (<>))
    (\s -> ite @(Expr BoolSort) (strLength s >? 0) (Just (strSubstring s 0 (strLength s - 1), strAt s (strLength s - 1))) Nothing)

type instance Index   (Expr (ArraySort k v)) = Expr k
type instance IxValue (Expr (ArraySort k v)) = Expr v

instance (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Ord (HaskellType v)) => Ixed (Expr (ArraySort k v)) where
  ix i f arr = f (select arr i) <&> store arr i

-- | __Caution for quantified expressions:__ 'uniplate1' will only be applied if quantification has taken place already.
instance Uniplate1 Expr '[KnownSMTSort] where
  uniplate1 _ expr@(Var _)            = pure expr
  uniplate1 _ expr@(Constant _)       = pure expr
  uniplate1 f (Plus x y)              = Plus <$> f x <*> f y
  uniplate1 f (Minus x y)             = Minus <$> f x <*> f y
  uniplate1 f (Neg x)                 = Neg <$> f x
  uniplate1 f (Mul x y)               = Mul <$> f x <*> f y
  uniplate1 f (Abs x)                 = Abs <$> f x
  uniplate1 f (Mod x y)               = Mod <$> f x <*> f y
  uniplate1 f (Rem x y)               = Rem <$> f x <*> f y
  uniplate1 f (IDiv x y)              = IDiv <$> f x <*> f y
  uniplate1 f (Div x y)               = Div <$> f x <*> f y
  uniplate1 f (LTH x y)               = LTH <$> f x <*> f y
  uniplate1 f (LTHE x y)              = LTHE <$> f x <*> f y
  uniplate1 f (EQU xs)                = EQU <$> traverse f xs
  uniplate1 f (Distinct xs)           = Distinct <$> traverse f xs
  uniplate1 f (GTHE x y)              = GTHE <$> f x <*> f y
  uniplate1 f (GTH x y)               = GTH <$> f x <*> f y
  uniplate1 f (Not x)                 = Not <$> f x
  uniplate1 f (And x y)               = And <$> f x <*> f y
  uniplate1 f (Or x y)                = Or <$> f x <*> f y
  uniplate1 f (Impl x y)              = Impl <$> f x <*> f y
  uniplate1 f (Xor x y)               = Xor <$> f x <*> f y
  uniplate1 _ Pi                      = pure Pi
  uniplate1 f (Sqrt x)                = Sqrt <$> f x
  uniplate1 f (Exp x)                 = Exp <$> f x
  uniplate1 f (Sin x)                 = Sin <$> f x
  uniplate1 f (Cos x)                 = Cos <$> f x
  uniplate1 f (Tan x)                 = Tan <$> f x
  uniplate1 f (Asin x)                = Asin <$> f x
  uniplate1 f (Acos x)                = Acos <$> f x
  uniplate1 f (Atan x)                = Atan <$> f x
  uniplate1 f (ToReal x)              = ToReal <$> f x
  uniplate1 f (ToInt x)               = ToInt <$> f x
  uniplate1 f (IsInt x)               = IsInt <$> f x
  uniplate1 f (Ite p t n)             = Ite <$> f p <*> f t <*> f n
  uniplate1 f (BvNand x y)            = BvNand <$> f x <*> f y
  uniplate1 f (BvNor x y)             = BvNor <$> f x <*> f y
  uniplate1 f (BvShL x y)             = BvShL <$> f x <*> f y
  uniplate1 f (BvLShR x y)            = BvLShR <$> f x <*> f y
  uniplate1 f (BvAShR x y)            = BvAShR <$> f x <*> f y
  uniplate1 f (BvConcat x y)          = BvConcat <$> f x <*> f y
  uniplate1 f (BvRotL i x)            = BvRotL i <$> f x
  uniplate1 f (BvRotR i x)            = BvRotR i <$> f x
  uniplate1 f (ArrSelect i arr)       = ArrSelect i <$> f arr
  uniplate1 f (ArrStore i x arr)      = ArrStore i <$> f x <*> f arr
  uniplate1 f (StrConcat x y)         = StrConcat <$> f x <*> f y
  uniplate1 f (StrLength x)           = StrLength <$> f x
  uniplate1 f (StrAt x i)             = StrAt <$> f x <*> f i
  uniplate1 f (StrSubstring x i j)    = StrSubstring <$> f x <*> f i <*> f j
  uniplate1 f (StrPrefixOf x y)       = StrPrefixOf <$> f x <*> f y
  uniplate1 f (StrSuffixOf x y)       = StrSuffixOf <$> f x <*> f y
  uniplate1 f (StrContains x y)       = StrContains <$> f x <*> f y
  uniplate1 f (StrIndexOf x y i)      = StrIndexOf <$> f x <*> f y <*> f i
  uniplate1 f (StrReplace x y y')     = StrReplace <$> f x <*> f y <*> f y'
  uniplate1 f (StrReplaceAll x y y')  = StrReplaceAll <$> f x <*> f y <*> f y'
  uniplate1 f (ForAll (Just qv) expr) = ForAll (Just qv) . const <$> f (expr (Var qv))
  uniplate1 _ (ForAll Nothing expr)   = pure $ ForAll Nothing expr
  uniplate1 f (Exists (Just qv) expr) = Exists (Just qv) . const <$> f (expr (Var qv))
  uniplate1 _ (Exists Nothing expr)   = pure $ Exists Nothing expr

-- | __Caution for quantified expressions:__ 'plate' will only be applied if quantification has taken place already.
instance KnownSMTSort t => Plated (Expr t) where
  plate f = uniplate1 (tryPlate f)
    where
      tryPlate :: forall s f. (KnownSMTSort s, Applicative f) => (Expr s -> f (Expr s)) -> (forall r. KnownSMTSort r => Expr r -> f (Expr r))
      tryPlate f' expr = case geq (sortSing @s) (sortSing' expr) of
        Just Refl -> f' expr
        Nothing   -> case expr of
          Var _                -> pure expr
          Constant _           -> pure expr
          Plus x y             -> Plus <$> tryPlate f' x <*> tryPlate f' y
          Minus x y            -> Minus <$> tryPlate f' x <*> tryPlate f' y
          Neg x                -> Neg  <$> tryPlate f' x
          Mul x y              -> Mul  <$> tryPlate f' x <*> tryPlate f' y
          Abs x                -> Abs  <$> tryPlate f' x
          Mod x y              -> Mod  <$> tryPlate f' x <*> tryPlate f' y
          Rem x y              -> Mod  <$> tryPlate f' x <*> tryPlate f' y
          IDiv x y             -> IDiv <$> tryPlate f' x <*> tryPlate f' y
          Div x y              -> Div  <$> tryPlate f' x <*> tryPlate f' y
          LTH x y              -> LTH  <$> tryPlate f' x <*> tryPlate f' y
          LTHE x y             -> LTHE <$> tryPlate f' x <*> tryPlate f' y
          EQU xs               -> EQU  <$> traverse (tryPlate f') xs
          Distinct xs          -> Distinct <$> traverse (tryPlate f') xs
          GTHE x y             -> GTHE <$> tryPlate f' x <*> tryPlate f' y
          GTH x y              -> GTH  <$> tryPlate f' x <*> tryPlate f' y
          Not x                -> Not  <$> tryPlate f' x
          And x y              -> And  <$> tryPlate f' x <*> tryPlate f' y
          Or x y               -> Or   <$> tryPlate f' x <*> tryPlate f' y
          Impl x y             -> Impl <$> tryPlate f' x <*> tryPlate f' y
          Xor x y              -> Xor  <$> tryPlate f' x <*> tryPlate f' y
          Pi                   -> pure Pi
          Sqrt x               -> Sqrt <$> tryPlate f' x
          Exp x                -> Exp  <$> tryPlate f' x
          Sin x                -> Sin  <$> tryPlate f' x
          Cos x                -> Cos  <$> tryPlate f' x
          Tan x                -> Tan  <$> tryPlate f' x
          Asin x               -> Asin <$> tryPlate f' x
          Acos x               -> Acos <$> tryPlate f' x
          Atan x               -> Atan <$> tryPlate f' x
          ToReal x             -> ToReal <$> tryPlate f' x
          ToInt x              -> ToInt  <$> tryPlate f' x
          IsInt x              -> IsInt  <$> tryPlate f' x
          Ite p t n            -> Ite    <$> tryPlate f' p <*> tryPlate f' t <*> tryPlate f' n
          BvNand x y           -> BvNand <$> tryPlate f' x <*> tryPlate f' y
          BvNor x y            -> BvNor  <$> tryPlate f' x <*> tryPlate f' y
          BvShL x y            -> BvShL  <$> tryPlate f' x <*> tryPlate f' y
          BvLShR x y           -> BvLShR <$> tryPlate f' x <*> tryPlate f' y
          BvAShR x y           -> BvAShR <$> tryPlate f' x <*> tryPlate f' y
          BvConcat x y         -> BvConcat <$> tryPlate f' x <*> tryPlate f' y
          BvRotL i x           -> BvRotL i <$> tryPlate f' x
          BvRotR i x           -> BvRotR i <$> tryPlate f' x
          ArrSelect i arr      -> ArrSelect i   <$> tryPlate f' arr
          ArrStore i x arr     -> ArrStore i    <$> tryPlate f' x <*> tryPlate f' arr
          StrConcat x y        -> StrConcat     <$> tryPlate f' x <*> tryPlate f' y
          StrLength x          -> StrLength     <$> tryPlate f' x
          StrAt x i            -> StrAt         <$> tryPlate f' x <*> tryPlate f' i
          StrSubstring x i j   -> StrSubstring  <$> tryPlate f' x <*> tryPlate f' i <*> tryPlate f' j
          StrPrefixOf x y      -> StrPrefixOf   <$> tryPlate f' x <*> tryPlate f' y
          StrSuffixOf x y      -> StrSuffixOf   <$> tryPlate f' x <*> tryPlate f' y
          StrContains x y      -> StrContains   <$> tryPlate f' x <*> tryPlate f' y
          StrIndexOf x y i     -> StrIndexOf    <$> tryPlate f' x <*> tryPlate f' y <*> tryPlate f' i
          StrReplace x y y'    -> StrReplace    <$> tryPlate f' x <*> tryPlate f' y <*> tryPlate f' y'
          StrReplaceAll x y y' -> StrReplaceAll <$> tryPlate f' x <*> tryPlate f' y <*> tryPlate f' y'
          ForAll (Just qv) qexpr -> ForAll (Just qv) . const <$> tryPlate f' (qexpr (Var qv))
          ForAll Nothing qexpr   -> pure $ ForAll Nothing qexpr
          Exists (Just qv) qexpr -> Exists (Just qv) . const <$> tryPlate f' (qexpr (Var qv))
          Exists Nothing qexpr   -> pure $ Exists Nothing qexpr

instance GNFData Expr where
  grnf expr = case expr of
    Var (SMTVar vId)     -> vId `seq` ()
    Constant c           -> c `seq` ()
    Plus e1 e2           -> grnf e1 `seq` grnf e2
    Minus e1 e2          -> grnf e1 `seq` grnf e2
    Neg e                -> grnf e
    Mul e1 e2            -> grnf e1 `seq` grnf e2
    Abs e                -> grnf e
    Mod e1 e2            -> grnf e1 `seq` grnf e2
    Rem e1 e2            -> grnf e1 `seq` grnf e2
    IDiv e1 e2           -> grnf e1 `seq` grnf e2
    Div e1 e2            -> grnf e1 `seq` grnf e2
    LTH e1 e2            -> grnf e1 `seq` grnf e2
    LTHE e1 e2           -> grnf e1 `seq` grnf e2
    EQU vec              -> vec `seq` V.foldl' (const grnf) () vec
    Distinct vec         -> vec `seq` V.foldl' (const grnf) () vec
    GTHE e1 e2           -> grnf e1 `seq` grnf e2
    GTH e1 e2            -> grnf e1 `seq` grnf e2
    Not e                -> grnf e
    And e1 e2            -> grnf e1 `seq` grnf e2
    Or e1 e2             -> grnf e1 `seq` grnf e2
    Impl e1 e2           -> grnf e1 `seq` grnf e2
    Xor e1 e2            -> grnf e1 `seq` grnf e2
    Pi                   -> ()
    Sqrt e               -> grnf e
    Exp e                -> grnf e
    Sin e                -> grnf e
    Cos e                -> grnf e
    Tan e                -> grnf e
    Asin e               -> grnf e
    Acos e               -> grnf e
    Atan e               -> grnf e
    ToReal e             -> grnf e
    ToInt e              -> grnf e
    IsInt e              -> grnf e
    Ite c e1 e2          -> grnf c `seq` grnf e1 `seq` grnf e2
    BvNand e1 e2         -> grnf e1 `seq` grnf e2
    BvNor e1 e2          -> grnf e1 `seq` grnf e2
    BvShL e1 e2          -> grnf e1 `seq` grnf e2
    BvLShR e1 e2         -> grnf e1 `seq` grnf e2
    BvAShR e1 e2         -> grnf e1 `seq` grnf e2
    BvConcat e1 e2       -> grnf e1 `seq` grnf e2
    BvRotL _ e           -> grnf e
    BvRotR _ e           -> grnf e
    ArrSelect e1 e2      -> grnf e1 `seq` grnf e2
    ArrStore e1 e2 e3    -> grnf e1 `seq` grnf e2 `seq` grnf e3
    StrConcat e1 e2      -> grnf e1 `seq` grnf e2
    StrLength e          -> grnf e
    StrAt e1 e2          -> grnf e1 `seq` grnf e2
    StrSubstring e1 e2 e3 -> grnf e1 `seq` grnf e2 `seq` grnf e3
    StrPrefixOf e1 e2    -> grnf e1 `seq` grnf e2
    StrSuffixOf e1 e2    -> grnf e1 `seq` grnf e2
    StrContains e1 e2    -> grnf e1 `seq` grnf e2
    StrIndexOf e1 e2 e3  -> grnf e1 `seq` grnf e2 `seq` grnf e3
    StrReplace e1 e2 e3  -> grnf e1 `seq` grnf e2 `seq` grnf e3
    StrReplaceAll e1 e2 e3 -> grnf e1 `seq` grnf e2 `seq` grnf e3
    ForAll Nothing _     -> ()
    ForAll (Just qv) f   -> grnf $ f $ Var qv
    Exists Nothing _     -> ()
    Exists (Just qv) f   -> grnf $ f $ Var qv

instance Eq (Expr t) where
  (==) = defaultEq

instance Ord (Expr t) where
  compare = defaultCompare

instance GEq Expr where
  geq = defaultGeq

gcomparing :: GCompare f => [(f a, f b)] -> GOrdering a b
gcomparing [] = GLT
gcomparing ((x,y):xys) = case gcompare x y of
  GEQ -> gcomparing xys
  o -> o

instance GCompare Expr where
  gcompare (Var v) (Var v') = case gcompare (sortSing' v) (sortSing' v') of
    GLT -> GLT
    GEQ -> case compare (coerce @_ @Int v) (coerce v') of
      LT -> GLT
      EQ -> GEQ
      GT -> GGT
    GGT -> GGT
  gcompare (Var _) _ = GLT
  gcompare _ (Var _) = GGT
  gcompare (Constant c) (Constant c') = gcompare c c'
  gcompare (Constant _) _ = GLT
  gcompare _ (Constant _) = GGT
  gcompare (Plus x y) (Plus x' y') = gcomparing [(x,x'), (y,y')]
  gcompare (Plus _ _) _ = GLT
  gcompare _ (Plus _ _) = GGT
  gcompare (Minus x y) (Minus x' y') = gcomparing [(x,x'), (y,y')]
  gcompare (Minus _ _) _ = GLT
  gcompare _ (Minus _ _) = GGT
  gcompare (Neg x) (Neg x') = gcompare x x'
  gcompare (Neg _) _ = GLT
  gcompare _ (Neg _) = GGT
  gcompare (Mul x y) (Mul x' y') = gcomparing [(x,x'), (y,y')]
  gcompare (Mul _ _) _ = GLT
  gcompare _ (Mul _ _) = GGT
  gcompare (Abs x) (Abs x') = gcompare x x'
  gcompare (Abs _) _ = GLT
  gcompare _ (Abs _) = GGT
  gcompare (Mod x y) (Mod x' y') = gcomparing [(x,x'), (y,y')]
  gcompare (Mod _ _) _ = GLT
  gcompare _ (Mod _ _) = GGT
  gcompare (Rem x y) (Rem x' y') = gcomparing [(x,x'), (y,y')]
  gcompare (Rem _ _) _ = GLT
  gcompare _ (Rem _ _) = GGT
  gcompare (IDiv x y) (IDiv x' y') = gcomparing [(x,x'), (y,y')]
  gcompare (IDiv _ _) _ = GLT
  gcompare _ (IDiv _ _) = GGT
  gcompare (Div x y) (Div x' y') = gcomparing [(x,x'), (y,y')]
  gcompare (Div _ _) _ = GLT
  gcompare _ (Div _ _) = GGT
  gcompare (LTH x y)               (LTH x' y')             = case gcomparing [(x,x'), (y,y')] of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  gcompare (LTH _ _) _ = GLT
  gcompare _ (LTH _ _) = GGT
  gcompare (LTHE x y)              (LTHE x' y')            = case gcomparing [(x,x'), (y,y')] of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  gcompare (LTHE _ _) _ = GLT
  gcompare _ (LTHE _ _) = GGT
  gcompare (EQU (V.toList -> xs))  (EQU (V.toList -> xs')) = case compare (length xs ) (length xs') of
    LT -> GLT
    EQ -> case gcomparing $ zip xs xs' of
      GGT -> GGT
      GEQ -> GEQ
      GLT -> GLT
    GT -> GGT
  gcompare (EQU _) _ = GLT
  gcompare _ (EQU _) = GGT
  gcompare (Distinct (V.toList -> xs)) (Distinct (V.toList -> xs')) = case compare (length xs ) (length xs') of
    LT -> GLT
    EQ -> case gcomparing $ zip xs xs' of
      GGT -> GGT
      GEQ -> GEQ
      GLT -> GLT
    GT -> GGT
  gcompare (Distinct _) _ = GLT
  gcompare _ (Distinct _) = GGT
  gcompare (GTHE x y)              (GTHE x' y')            = case gcomparing [(x,x'), (y,y')] of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  gcompare (GTHE _ _) _ = GLT
  gcompare _ (GTHE _ _) = GGT
  gcompare (GTH x y)               (GTH x' y')             = case gcomparing [(x,x'), (y,y')] of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  gcompare (GTH _ _) _ = GLT
  gcompare _ (GTH _ _) = GGT
  gcompare (Not x)                 (Not x')                = gcompare x x'
  gcompare (Not _) _ = GLT
  gcompare _ (Not _) = GGT
  gcompare (And x y)               (And x' y')             = gcomparing [(x,x'), (y,y')]
  gcompare (And _ _) _ = GLT
  gcompare _ (And _ _) = GGT
  gcompare (Or x y)                (Or x' y')              = gcomparing [(x,x'), (y,y')]
  gcompare (Or _ _) _ = GLT
  gcompare _ (Or _ _) = GGT
  gcompare (Impl x y)              (Impl x' y')            = gcomparing [(x,x'), (y,y')]
  gcompare (Impl _ _) _ = GLT
  gcompare _ (Impl _ _) = GGT
  gcompare (Xor x y)               (Xor x' y')             = gcomparing [(x,x'), (y,y')]
  gcompare (Xor _ _) _ = GLT
  gcompare _ (Xor _ _) = GGT
  gcompare Pi                      Pi                      = GEQ
  gcompare Pi _ = GLT
  gcompare _ Pi = GGT
  gcompare (Sqrt x)                (Sqrt x')               = gcompare x x'
  gcompare (Sqrt _) _ = GLT
  gcompare _ (Sqrt _) = GGT
  gcompare (Exp x)                 (Exp x')                = gcompare x x'
  gcompare (Exp _) _ = GLT
  gcompare _ (Exp _) = GGT
  gcompare (Sin x)                 (Sin x')                = gcompare x x'
  gcompare (Sin _) _ = GLT
  gcompare _ (Sin _) = GGT
  gcompare (Cos x)                 (Cos x')                = gcompare x x'
  gcompare (Cos _) _ = GLT
  gcompare _ (Cos _) = GGT
  gcompare (Tan x)                 (Tan x')                = gcompare x x'
  gcompare (Tan _) _ = GLT
  gcompare _ (Tan _) = GGT
  gcompare (Asin x)                (Asin x')               = gcompare x x'
  gcompare (Asin _) _ = GLT
  gcompare _ (Asin _) = GGT
  gcompare (Acos x)                (Acos x')               = gcompare x x'
  gcompare (Acos _) _ = GLT
  gcompare _ (Acos _) = GGT
  gcompare (Atan x)                (Atan x')               = gcompare x x'
  gcompare (Atan _) _ = GLT
  gcompare _ (Atan _) = GGT
  gcompare (ToReal x)              (ToReal x')             = case gcompare x x' of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  gcompare (ToReal _) _ = GLT
  gcompare _ (ToReal _) = GGT
  gcompare (ToInt x)               (ToInt x')              = case gcompare x x' of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  gcompare (ToInt _) _ = GLT
  gcompare _ (ToInt _) = GGT
  gcompare (IsInt x)               (IsInt x')              = case gcompare x x' of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  gcompare (IsInt _) _ = GLT
  gcompare _ (IsInt _) = GGT
  gcompare (Ite p t n)             (Ite p' t' n')          = case gcompare p p' of
    GLT -> GLT
    GEQ -> gcomparing [(t,t'), (n,n')]
    GGT -> GGT
  gcompare (Ite _ _ _) _ = GLT
  gcompare _ (Ite _ _ _) = GGT
  gcompare (BvNand x y)            (BvNand x' y')          = gcomparing [(x,x'), (y,y')]
  gcompare (BvNand _ _) _ = GLT
  gcompare _ (BvNand _ _) = GGT
  gcompare (BvNor x y)             (BvNor x' y')           = gcomparing [(x,x'), (y,y')]
  gcompare (BvNor _ _) _ = GLT
  gcompare _ (BvNor _ _) = GGT
  gcompare (BvShL x y)             (BvShL x' y')           = gcomparing [(x,x'), (y,y')]
  gcompare (BvShL _ _) _ = GLT
  gcompare _ (BvShL _ _) = GGT
  gcompare (BvLShR x y)            (BvLShR x' y')          = gcomparing [(x,x'), (y,y')]
  gcompare (BvLShR _ _) _ = GLT
  gcompare _ (BvLShR _ _) = GGT
  gcompare (BvAShR x y)            (BvAShR x' y')          = gcomparing [(x,x'), (y,y')]
  gcompare (BvAShR _ _) _ = GLT
  gcompare _ (BvAShR _ _) = GGT
  gcompare (BvConcat x y)          (BvConcat x' y')        = case gcompare (sortSing' x) (sortSing' x') of
    GLT -> GLT
    GEQ -> case gcompare x x' of
      GLT -> GLT
      GEQ -> case gcompare (sortSing' y) (sortSing' y') of
        GLT -> GLT
        GEQ -> case gcompare y y' of
          GLT -> GLT
          GEQ -> GEQ
          GGT -> GGT
        GGT -> GGT
      GGT -> GGT
    GGT -> GGT
  gcompare (BvConcat _ _) _ = GLT
  gcompare _ (BvConcat _ _) = GGT
  gcompare (BvRotL i x)            (BvRotL i' x')          = case compare (fromIntegral i :: Integer) (fromIntegral i') of
    LT -> GLT
    EQ -> gcompare x x'
    GT -> GGT
  gcompare (BvRotL _ _) _ = GLT
  gcompare _ (BvRotL _ _) = GGT
  gcompare (BvRotR i x)            (BvRotR i' x')          = case compare (fromIntegral i :: Integer) (fromIntegral i') of
    LT -> GLT
    EQ -> gcompare x x'
    GT -> GGT
  gcompare (BvRotR _ _) _ = GLT
  gcompare _ (BvRotR _ _) = GGT
  gcompare (ArrSelect arr i)       (ArrSelect arr' i')     = case gcompare arr arr' of
    GLT -> GLT
    GEQ -> case gcompare i i' of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT
    GGT -> GGT
  gcompare (ArrSelect _ _) _ = GLT
  gcompare _ (ArrSelect _ _) = GGT
  gcompare (ArrStore arr k v)      (ArrStore arr' k' v')   = case gcompare arr arr' of
    GLT -> GLT
    GEQ -> case gcompare k k' of
      GLT -> GLT
      GEQ -> case gcompare v v' of
        GLT -> GLT
        GEQ -> GEQ
        GGT -> GGT
      GGT -> GGT
    GGT -> GGT
  gcompare (ArrStore _ _ _) _ = GLT
  gcompare _ (ArrStore _ _ _) = GGT
  gcompare (StrConcat x y)         (StrConcat x' y')       = gcomparing [(x,x'), (y,y')]
  gcompare (StrConcat _ _) _ = GLT
  gcompare _ (StrConcat _ _) = GGT
  gcompare (StrLength x)           (StrLength x')          = case gcompare x x' of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  gcompare (StrLength _) _ = GLT
  gcompare _ (StrLength _) = GGT
  gcompare (StrAt x i)             (StrAt x' i')           = case gcompare x x' of
    GLT -> GLT
    GEQ -> case gcompare i i' of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT
    GGT -> GGT
  gcompare (StrAt _ _) _ = GLT
  gcompare _ (StrAt _ _) = GGT
  gcompare (StrSubstring x i j)    (StrSubstring x' i' j') = case gcompare x x' of
    GLT -> GLT
    GEQ -> case gcomparing [(i,i'), (j,j')] of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT
    GGT -> GGT
  gcompare (StrSubstring _ _ _) _ = GLT
  gcompare _ (StrSubstring _ _ _) = GGT
  gcompare (StrPrefixOf x y)       (StrPrefixOf x' y')     = case gcomparing [(x,x'), (y,y')] of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  gcompare (StrPrefixOf _ _) _ = GLT
  gcompare _ (StrPrefixOf _ _) = GGT
  gcompare (StrSuffixOf x y)       (StrSuffixOf x' y')     = case gcomparing [(x,x'), (y,y')] of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  gcompare (StrSuffixOf _ _) _ = GLT
  gcompare _ (StrSuffixOf _ _) = GGT
  gcompare (StrContains x y)       (StrContains x' y')     = case gcomparing [(x,x'), (y,y')] of
    GLT -> GLT
    GEQ -> GEQ
    GGT -> GGT
  gcompare (StrContains _ _) _ = GLT
  gcompare _ (StrContains _ _) = GGT
  gcompare (StrIndexOf x y i)      (StrIndexOf x' y' i')   = case gcomparing [(x,x'), (y,y')] of
    GLT -> GLT
    GEQ -> gcompare i i'
    GGT -> GGT
  gcompare (StrIndexOf _ _ _) _ = GLT
  gcompare _ (StrIndexOf _ _ _) = GGT
  gcompare (StrReplace source target replacement)     (StrReplace source' target' replacement')     = gcomparing [(source, source'), (target, target'), (replacement, replacement')]
  gcompare (StrReplace _ _ _) _ = GLT
  gcompare _ (StrReplace _ _ _) = GGT
  gcompare (StrReplaceAll source target replacement)  (StrReplaceAll source' target' replacement')  = gcomparing [(source, source'), (target, target'), (replacement, replacement')]
  gcompare (StrReplaceAll _ _ _) _ = GLT
  gcompare _ (StrReplaceAll _ _ _) = GGT
  gcompare (ForAll _ expr)         (ForAll _ expr')        = gcompare (expr $ Var (SMTVar (-1))) (expr' $ Var (SMTVar (-1)))
  gcompare (ForAll _ _) _ = GLT
  gcompare _ (ForAll _ _) = GGT
  gcompare (Exists _ expr)         (Exists _ expr')        = gcompare (expr $ Var (SMTVar (-1))) (expr' $ Var (SMTVar (-1)))
  -- gcompare (Exists _ _) _ = GLT
  -- gcompare _ (Exists _ _) = GGT
