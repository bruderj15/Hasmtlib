{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies #-}

module Language.Hasmtlib.Type.Expr where

import Prelude hiding (Integral(..), not, and, or, any, all, (&&), (||))
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Internal.Uniplate1
import Language.Hasmtlib.Type.ArrayMap
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Integraled
import Language.Hasmtlib.Boolean
import Data.GADT.Compare
import Data.GADT.DeepSeq
import Data.Map hiding (toList)
import Data.Proxy
import Data.Coerce
import Data.Int
import Data.Word
import Data.Void
import Data.Sequence (Seq)
import Data.Tree (Tree)
import Data.Monoid (Sum, Product, First, Last, Dual)
import Data.String (IsString(..))
import Data.Text (pack)
import Data.List(genericLength)
import Data.Foldable (toList)
import Data.ByteString.Builder
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.Vector.Sized as V
import Control.Lens hiding (from, to)
import GHC.TypeLits hiding (someNatVal)
import GHC.TypeNats (someNatVal)
import GHC.Generics

-- | An internal SMT variable with a phantom-type which holds an 'Int' as it's identifier.
type role SMTVar phantom
newtype SMTVar (t :: SMTSort) = SMTVar { _varId :: Int }
  deriving stock (Show, Generic)
  deriving newtype (Eq, Ord)
$(makeLenses ''SMTVar)

-- | A wrapper for values of 'SMTSort's.
data Value (t :: SMTSort) where
  IntValue    :: HaskellType IntSort    -> Value IntSort
  RealValue   :: HaskellType RealSort   -> Value RealSort
  BoolValue   :: HaskellType BoolSort   -> Value BoolSort
  BvValue     :: KnownNat n => HaskellType (BvSort n) -> Value (BvSort n)
  ArrayValue  :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Eq (HaskellType v)) => HaskellType (ArraySort k v) -> Value (ArraySort k v)
  StringValue :: HaskellType StringSort -> Value StringSort

deriving instance Eq (HaskellType t) => Eq (Value t)
deriving instance Ord (HaskellType t) => Ord (Value t)

instance GEq Value where
  geq (BoolValue x) (BoolValue y)   = if x == y then Just Refl else Nothing
  geq (IntValue x) (IntValue y)     = if x == y then Just Refl else Nothing
  geq (RealValue x) (RealValue y)   = if x == y then Just Refl else Nothing
  geq (BvValue x) (BvValue y)       = case cmpNat x y of
    EQI -> if x == y then Just Refl else Nothing
    _   -> Nothing
  geq ax@(ArrayValue x) ay@(ArrayValue y) = case geq (sortSing' ax) (sortSing' ay) of
    Nothing -> Nothing
    Just Refl -> if x == y then Just Refl else Nothing
  geq (StringValue x) (StringValue y) = if x == y then Just Refl else Nothing
  geq _ _ = Nothing

-- | Unwrap a value from 'Value'.
unwrapValue :: Value t -> HaskellType t
unwrapValue (IntValue  v)   = v
unwrapValue (RealValue v)   = v
unwrapValue (BoolValue v)   = v
unwrapValue (BvValue   v)   = v
unwrapValue (ArrayValue v)  = v
unwrapValue (StringValue v) = v
{-# INLINEABLE unwrapValue #-}

-- | Wrap a value into 'Value'.
wrapValue :: forall t. KnownSMTSort t => HaskellType t -> Value t
wrapValue = case sortSing @t of
  SIntSort       -> IntValue
  SRealSort      -> RealValue
  SBoolSort      -> BoolValue
  SBvSort _      -> BvValue
  SArraySort _ _ -> ArrayValue
  SStringSort    -> StringValue
{-# INLINEABLE wrapValue #-}

-- | An existential wrapper that hides some known 'SMTSort'.
type SomeKnownSMTSort f = SomeSMTSort '[KnownSMTSort] f

-- | Am SMT expression.
--   For internal use only.
--   For building expressions use the corresponding instances (Num, Boolean, ...).
data Expr (t :: SMTSort) where
  Var       :: SMTVar t -> Expr t
  Constant  :: Value  t -> Expr t

  Plus      :: Num (HaskellType t) => Expr t -> Expr t -> Expr t
  Neg       :: Num (HaskellType t) => Expr t -> Expr t
  Mul       :: Num (HaskellType t) => Expr t -> Expr t -> Expr t
  Abs       :: Num (HaskellType t) => Expr t -> Expr t
  Mod       :: Expr IntSort  -> Expr IntSort  -> Expr IntSort
  IDiv      :: Expr IntSort  -> Expr IntSort  -> Expr IntSort
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

  BvNot     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n)
  BvAnd     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvOr      :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvXor     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvNand    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvNor     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvNeg     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n)
  BvAdd     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvSub     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvMul     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvuDiv    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvuRem    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvShL     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvLShR    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
  BvConcat  :: (KnownNat n, KnownNat m) => Expr (BvSort n) -> Expr (BvSort m) -> Expr (BvSort (n + m))
  BvRotL    :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
  BvRotR    :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
  BvuLT     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort
  BvuLTHE   :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort
  BvuGTHE   :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort
  BvuGT     :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr BoolSort

  ArrSelect :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Eq (HaskellType v)) => Expr (ArraySort k v) -> Expr k -> Expr v
  ArrStore  :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v -> Expr (ArraySort k v)

  StrConcat     :: Expr StringSort -> Expr StringSort -> Expr StringSort
  StrLength     :: Expr StringSort -> Expr IntSort
  StrLT         :: Expr StringSort -> Expr StringSort -> Expr BoolSort
  StrLTHE       :: Expr StringSort -> Expr StringSort -> Expr BoolSort
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
  --   All non-recursive contructors form leafs.
isLeaf :: Expr t -> Bool
isLeaf (Var _) = True
isLeaf (Constant _) = True
isLeaf Pi = True
isLeaf _ = False
{-# INLINE isLeaf #-}

-- | If condition (p :: b) then (t :: a) else (f :: a)
--
--    >>> ite true "1" "2"
--        "1"
--    >>> ite false 100 42
--        42
class Iteable b a where
  ite :: b -> a -> a -> a
  default ite :: (Iteable b c, Applicative f, f c ~ a) => b -> a -> a -> a
  ite p = liftA2 (ite p)

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

-- | Compare two as as SMT-Expression.
--
--   You can derive an instance of this class if your type is 'Generic'.
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

-- | Minimum of two as SMT-Expression.
min' :: (Orderable a, Iteable (Expr BoolSort) a) => a -> a -> a
min' x y = ite (x <=? y) x y

-- | Maximum of two as SMT-Expression.
max' :: (Orderable a, Iteable (Expr BoolSort) a) => a -> a -> a
max' x y = ite (y <=? x) x y

instance Orderable (Expr IntSort) where
  (<?)     = LTH
  {-# INLINE (<?) #-}
  (<=?)    = LTHE
  {-# INLINE (<=?) #-}
  (>=?)    = GTHE
  {-# INLINE (>=?) #-}
  (>?)     = GTH
  {-# INLINE (>?) #-}

instance Orderable (Expr RealSort) where
  (<?)     = LTH
  {-# INLINE (<?) #-}
  (<=?)    = LTHE
  {-# INLINE (<=?) #-}
  (>=?)    = GTHE
  {-# INLINE (>=?) #-}
  (>?)     = GTH
  {-# INLINE (>?) #-}

instance KnownNat n => Orderable (Expr (BvSort n)) where
  (<?)     = BvuLT
  {-# INLINE (<?) #-}
  (<=?)    = BvuLTHE
  {-# INLINE (<=?) #-}
  (>=?)    = BvuGTHE
  {-# INLINE (>=?) #-}
  (>?)     = BvuGT
  {-# INLINE (>?) #-}

-- | Lexicographic ordering for '(<?)' and reflexive closure of lexicographic ordering for '(<=?)'
instance Orderable (Expr StringSort) where
  (<?)     = StrLT
  {-# INLINE (<?) #-}
  (<=?)    = StrLTHE
  {-# INLINE (<=?) #-}

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

-- | Test multiple expressions on equality within in the 'SMT'-Problem.
equal :: (Eq (HaskellType t), KnownSMTSort t, Foldable f) => f (Expr t) -> Expr BoolSort
equal (toList -> (a:b:xs)) = case someNatVal (genericLength xs) of
  SomeNat n -> case V.fromListN' n xs of
    Nothing  -> EQU $ V.fromTuple (a,b)
    Just xs' -> EQU $ xs' V.++ V.fromTuple (a,b)
equal (toList -> _)        = true

-- | Test multiple expressions on distinctness within in the 'SMT'-Problem.
distinct :: (Eq (HaskellType t), KnownSMTSort t, Foldable f) => f (Expr t) -> Expr BoolSort
distinct (toList -> (a:b:xs)) = case someNatVal (genericLength xs) of
  SomeNat n -> case V.fromListN' n xs of
    Nothing  -> Distinct $ V.fromTuple (a,b)
    Just xs' -> Distinct $ xs' V.++ V.fromTuple (a,b)
distinct (toList -> _)        = true

-- | A universal quantification for any specific 'SMTSort'.
--   If the type cannot be inferred, apply a type-annotation.
--   Nested quantifiers are also supported.
--
--   Usage:
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

-- | An existential quantification for any specific 'SMTSort'
--   If the type cannot be inferred, apply a type-annotation.
--   Nested quantifiers are also supported.
--
--   Usage:
--
--   @
--   assert $
--      for_all @(BvSort 8) $ \x ->
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
select :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Eq (HaskellType v)) => Expr (ArraySort k v) -> Expr k -> Expr v
select = ArrSelect
{-# INLINE select #-}

-- | Store a value in an array.
store :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v -> Expr (ArraySort k v)
store = ArrStore
{-# INLINE store #-}

-- | Bitvector shift left
bvShL    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
bvShL    = BvShL
{-# INLINE bvShL #-}

-- | Bitvector logical shift right
bvLShR   :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
bvLShR   = BvLShR
{-# INLINE bvLShR #-}

-- | Concat two bitvectors
bvConcat :: (KnownNat n, KnownNat m) => Expr (BvSort n) -> Expr (BvSort m) -> Expr (BvSort (n + m))
bvConcat = BvConcat
{-# INLINE bvConcat #-}

-- | Rotate bitvector left
bvRotL   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
bvRotL   = BvRotL
{-# INLINE bvRotL #-}

-- | Rotate bitvector right
bvRotR   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
bvRotR   = BvRotR
{-# INLINE bvRotR #-}

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
--   @(str.prefixof s t)@ is @true@ iff @s@ is a prefix of @t@.
strPrefixOf :: Expr StringSort -> Expr StringSort -> Expr BoolSort
strPrefixOf = StrPrefixOf
{-# INLINE strPrefixOf #-}

-- | First string is a suffix of second one.
--   @(str.suffixof s t)@ is @true@ iff @s@ is a suffix of @t@.
strSuffixOf :: Expr StringSort -> Expr StringSort -> Expr BoolSort
strSuffixOf = StrSuffixOf
{-# INLINE strSuffixOf #-}

-- | First string contains second one
--   @(str.contains s t)@ iff @s@ contains @t@.
strContains :: Expr StringSort -> Expr StringSort -> Expr BoolSort
strContains = StrContains
{-# INLINE strContains #-}

-- | Index of first occurrence of second string in first one starting at the position specified by the third argument.
--   @(str.indexof s t i)@, with @0 <= i <= |s|@ is the position of the first
--   occurrence of @t@ in @s@ at or after position @i@, if any.
--   Otherwise, it is @-1@. Note that the result is @i@ whenever @i@ is within
--   the range @[0, |s|]@ and @t@ is empty.
strIndexOf :: Expr StringSort -> Expr StringSort -> Expr IntSort -> Expr IntSort
strIndexOf = StrIndexOf
{-# INLINE strIndexOf #-}

-- | @(str.replace s t t')@ is the string obtained by replacing the first
--   occurrence of @t@ in @s@, if any, by @t'@. Note that if @t@ is empty, the
--   result is to prepend @t'@ to @s@; also, if @t@ does not occur in @s@ then
--   the result is @s@.
strReplace :: Expr StringSort -> Expr StringSort -> Expr StringSort -> Expr StringSort
strReplace = StrReplace
{-# INLINE strReplace #-}

-- | @(str.replace_all s t t’)@ is @s@ if @t@ is the empty string. Otherwise, it
--   is the string obtained from @s@ by replacing all occurrences of @t@ in @s@
--   by @t’@, starting with the first occurrence and proceeding in left-to-right order.
strReplaceAll :: Expr StringSort -> Expr StringSort -> Expr StringSort -> Expr StringSort
strReplaceAll = StrReplaceAll
{-# INLINE strReplaceAll #-}

instance Num (Expr IntSort) where
   fromInteger = Constant . IntValue
   {-# INLINE fromInteger #-}
   (Constant (IntValue 0)) + y = y
   x + (Constant (IntValue 0)) = x
   (Constant (IntValue x)) + (Constant (IntValue y)) = Constant (IntValue (x + y))
   x + y = Plus x y
   {-# INLINE (+) #-}
   x - (Constant (IntValue 0)) = x
   (Constant (IntValue x)) - (Constant (IntValue y)) = Constant (IntValue (x - y))
   x - y = Plus x (Neg y)
   {-# INLINE (-) #-}
   (Constant (IntValue 0)) * _ = 0
   _ * (Constant (IntValue 0)) = 0
   (Constant (IntValue 1)) * y = y
   x * (Constant (IntValue 1)) = x
   (Constant (IntValue x)) * (Constant (IntValue y)) = Constant (IntValue (x * y))
   x * y = Mul x y
   {-# INLINE (*) #-}
   negate      = Neg
   {-# INLINE negate #-}
   abs         = Abs
   {-# INLINE abs #-}
   signum x    = ite (x === 0) 0 $ ite (x <? 0) (-1) 1
   {-# INLINE signum #-}

instance Num (Expr RealSort) where
   fromInteger = Constant . RealValue . fromIntegral
   {-# INLINE fromInteger #-}
   (Constant (RealValue 0)) + y = y
   x + (Constant (RealValue 0)) = x
   (Constant (RealValue x)) + (Constant (RealValue y)) = Constant (RealValue (x + y))
   x + y = Plus x y
   {-# INLINE (+) #-}
   x - (Constant (RealValue 0)) = x
   (Constant (RealValue x)) - (Constant (RealValue y)) = Constant (RealValue (x - y))
   x - y = Plus x (Neg y)
   {-# INLINE (-) #-}
   (Constant (RealValue 0)) * _ = 0
   _ * (Constant (RealValue 0)) = 0
   (Constant (RealValue 1)) * y = y
   x * (Constant (RealValue 1)) = x
   (Constant (RealValue x)) * (Constant (RealValue y)) = Constant (RealValue (x * y))
   x * y = Mul x y
   {-# INLINE (*) #-}
   negate      = Neg
   {-# INLINE negate #-}
   abs         = Abs
   {-# INLINE abs #-}
   signum x    = ite (x === 0) 0 $ ite (x <? 0) (-1) 1
   {-# INLINE signum #-}

instance KnownNat n => Num (Expr (BvSort n)) where
   fromInteger = Constant . BvValue . fromInteger
   {-# INLINE fromInteger #-}
   (Constant (BvValue 0)) + y = y
   x + (Constant (BvValue 0)) = x
   (Constant (BvValue x)) + (Constant (BvValue y)) = Constant (BvValue (x + y))
   x + y = BvAdd x y
   {-# INLINE (+) #-}
   x - (Constant (BvValue 0)) = x
   (Constant (BvValue x)) - (Constant (BvValue y)) = Constant (BvValue (x - y))
   x - y = BvSub x y
   {-# INLINE (-) #-}
   (Constant (BvValue 0)) * _ = 0
   _ * (Constant (BvValue 0)) = 0
   (Constant (BvValue 1)) * y = y
   x * (Constant (BvValue 1)) = x
   (Constant (BvValue x)) * (Constant (BvValue y)) = Constant (BvValue (x * y))
   x * y = BvMul x y
   {-# INLINE (*) #-}
   abs         = id
   {-# INLINE abs #-}
   signum _    = 0
   {-# INLINE signum #-}

instance Fractional (Expr RealSort) where
  fromRational = Constant . RealValue . fromRational
  {-# INLINE fromRational #-}
  x / (Constant (RealValue 1)) = x
  (Constant (RealValue 0)) / _ = 0
  (Constant (RealValue x)) / (Constant (RealValue y)) = Constant (RealValue (x / y))
  x / y          = Div x y
  {-# INLINE (/) #-}

-- | Not in the SMTLib2.6-standard. Solvers like CVC5 and MathSAT support it though.
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

instance Integraled (Expr IntSort) where
  quot = IDiv
  {-# INLINE quot #-}
  rem  = Mod
  {-# INLINE rem #-}
  div  = IDiv
  {-# INLINE div #-}
  mod  = Mod
  {-# INLINE mod #-}
  quotRem x y = (quot x y, rem x y)
  {-# INLINE quotRem #-}
  divMod x y  = (div x y, mod x y)
  {-# INLINE divMod #-}

instance KnownNat n => Integraled (Expr (BvSort n)) where
  quot        = BvuDiv
  {-# INLINE quot #-}
  rem         = BvuRem
  {-# INLINE rem #-}
  div         = BvuDiv
  {-# INLINE div #-}
  mod         = BvuRem
  {-# INLINE mod #-}
  quotRem x y = (quot x y, rem x y)
  {-# INLINE quotRem #-}
  divMod x y  = (div x y, mod x y)
  {-# INLINE divMod #-}

instance Boolean (Expr BoolSort) where
  bool = Constant . BoolValue
  {-# INLINE bool #-}
  (&&) = And
  {-# INLINE (&&) #-}
  (||) = Or
  {-# INLINE (||) #-}
  not  = Not
  {-# INLINE not #-}
  xor  = Xor
  {-# INLINE xor #-}
  (<==>) = (===)
  {-# INLINE (<==>) #-}

instance KnownNat n => Boolean (Expr (BvSort n)) where
  bool = Constant . BvValue . bool
  {-# INLINE bool #-}
  (&&) = BvAnd
  {-# INLINE (&&) #-}
  (||) = BvOr
  {-# INLINE (||) #-}
  not  = BvNot
  {-# INLINE not #-}
  xor  = BvXor
  {-# INLINE xor #-}

instance Bounded (Expr BoolSort) where
  minBound = false
  maxBound = true

instance KnownNat n => Bounded (Expr (BvSort n)) where
  minBound = Constant $ BvValue minBound
  maxBound = Constant $ BvValue maxBound

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

instance Render (SMTVar t) where
  render v = "var_" <> intDec (coerce @(SMTVar t) @Int v)
  {-# INLINE render #-}

instance Render (Value t) where
  render (IntValue x)   = render x
  render (RealValue x)  = render x
  render (BoolValue x)  = render x
  render (BvValue   v)  = "#b" <> render v
  render (ArrayValue arr) = case minViewWithKey (arr^.stored) of
    Nothing -> constRender $ arr^.arrConst
    Just ((k,v), stored')
      | size (arr^.stored) > 1 -> render $ ArrStore (Constant (wrapValue (arr & stored .~ stored'))) (Constant (wrapValue k)) (Constant (wrapValue v))
      | otherwise  -> constRender v
    where
      constRender v = "((as const " <> render (goSing arr) <> ") " <> render (wrapValue v) <> ")"
      goSing :: forall k v. (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Eq (HaskellType v)) => ConstArray (HaskellType k) (HaskellType v) -> SSMTSort (ArraySort k v)
      goSing _ = sortSing @(ArraySort k v)
  render (StringValue x) = "\"" <> render x <> "\""

instance KnownSMTSort t => Render (Expr t) where
  render (Var v)      = render v
  render (Constant c) = render c

  render (Plus x y)   = renderBinary "+" x y
  render (Neg x)      = renderUnary  "-" x
  render (Mul x y)    = renderBinary "*" x y
  render (Abs x)      = renderUnary  "abs" x
  render (Mod x y)    = renderBinary "mod" x y
  render (IDiv x y)   = renderBinary "div" x y
  render (Div x y)    = renderBinary "/" x y

  render (LTH x y)    = renderBinary "<" x y
  render (LTHE x y)   = renderBinary "<=" x y
  render (EQU xs)     = renderNary "=" $ V.toList xs
  render (Distinct xs)= renderNary "distinct" $ V.toList xs
  render (GTHE x y)   = renderBinary ">=" x y
  render (GTH x y)    = renderBinary ">" x y

  render (Not x)      = renderUnary  "not" x
  render (And x y)    = renderBinary "and" x y
  render (Or x y)     = renderBinary "or" x y
  render (Impl x y)   = renderBinary "=>" x y
  render (Xor x y)    = renderBinary "xor" x y

  render Pi           = "real.pi"
  render (Sqrt x)     = renderUnary "sqrt" x
  render (Exp x)      = renderUnary "exp" x
  render (Sin x)      = renderUnary "sin" x
  render (Cos x)      = renderUnary "cos" x
  render (Tan x)      = renderUnary "tan" x
  render (Asin x)     = renderUnary "arcsin" x
  render (Acos x)     = renderUnary "arccos" x
  render (Atan x)     = renderUnary "arctan" x

  render (ToReal x)   = renderUnary "to_real" x
  render (ToInt x)    = renderUnary "to_int" x
  render (IsInt x)    = renderUnary "is_int" x

  render (Ite p t f)  = renderTernary "ite" p t f

  render (BvNot x)          = renderUnary  "bvnot"  (render x)
  render (BvAnd x y)        = renderBinary "bvand"  (render x) (render y)
  render (BvOr x y)         = renderBinary "bvor"   (render x) (render y)
  render (BvXor x y)        = renderBinary "bvxor"  (render x) (render y)
  render (BvNand x y)       = renderBinary "bvnand" (render x) (render y)
  render (BvNor x y)        = renderBinary "bvnor"  (render x) (render y)
  render (BvNeg x)          = renderUnary  "bvneg"  (render x)
  render (BvAdd x y)        = renderBinary "bvadd"  (render x) (render y)
  render (BvSub x y)        = renderBinary "bvsub"  (render x) (render y)
  render (BvMul x y)        = renderBinary "bvmul"  (render x) (render y)
  render (BvuDiv x y)       = renderBinary "bvudiv" (render x) (render y)
  render (BvuRem x y)       = renderBinary "bvurem" (render x) (render y)
  render (BvShL x y)        = renderBinary "bvshl"  (render x) (render y)
  render (BvLShR x y)       = renderBinary "bvlshr" (render x) (render y)
  render (BvConcat x y)     = renderBinary "concat" (render x) (render y)
  render (BvRotL i x)       = renderUnary (renderBinary "_" ("rotate_left"  :: Builder) (render (natVal i))) (render x)
  render (BvRotR i x)       = renderUnary (renderBinary "_" ("rotate_right" :: Builder) (render (natVal i))) (render x)
  render (BvuLT x y)        = renderBinary "bvult"  (render x) (render y)
  render (BvuLTHE x y)      = renderBinary "bvule"  (render x) (render y)
  render (BvuGTHE x y)      = renderBinary "bvuge"  (render x) (render y)
  render (BvuGT x y)        = renderBinary "bvugt"  (render x) (render y)

  render (ArrSelect a i)    = renderBinary  "select" (render a) (render i)
  render (ArrStore a i v)   = renderTernary "store"  (render a) (render i) (render v)

  render (StrConcat x y)        = renderBinary "str.++"  (render x) (render y)
  render (StrLength x)          = renderUnary  "str.len" (render x)
  render (StrLT x y)            = renderBinary "str.<"   (render x) (render y)
  render (StrLTHE x y)          = renderBinary "str.<="  (render x) (render y)
  render (StrAt x i)            = renderBinary "str.at"  (render x) (render i)
  render (StrSubstring x i j)   = renderTernary "str.substr"  (render x) (render i) (render j)
  render (StrPrefixOf x y)      = renderBinary "str.prefixof" (render x) (render y)
  render (StrSuffixOf x y)      = renderBinary "str.suffixof" (render x) (render y)
  render (StrContains x y)      = renderBinary "str.contains" (render x) (render y)
  render (StrIndexOf x y i)     = renderTernary "str.indexof"     (render x) (render y) (render i)
  render (StrReplace x y y')    = renderTernary "str.replace"     (render x) (render y) (render y')
  render (StrReplaceAll x y y') = renderTernary "str.replace_all" (render x) (render y) (render y')

  render (ForAll mQvar f) = renderQuantifier "forall" mQvar f
  render (Exists mQvar f) = renderQuantifier "exists" mQvar f

renderQuantifier :: forall t. KnownSMTSort t => Builder -> Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Builder
renderQuantifier qname (Just qvar) f =
  renderBinary
    qname
    ("(" <> renderUnary (render qvar) (sortSing @t) <> ")")
    expr
  where
    expr = render $ f $ Var qvar
renderQuantifier _ Nothing _ = mempty

instance Show (Value t) where
  show = toString . toLazyByteString . render

instance KnownSMTSort t => Show (Expr t) where
  show = toString . toLazyByteString . render

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

instance (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Eq (HaskellType v)) => Ixed (Expr (ArraySort k v)) where
  ix i f arr = f (select arr i) <&> store arr i

-- | __Caution for quantified expressions:__ 'uniplate1' will only be applied if quantification has taken place already.
instance Uniplate1 Expr '[KnownSMTSort] where
  uniplate1 _ expr@(Var _)            = pure expr
  uniplate1 _ expr@(Constant _)       = pure expr
  uniplate1 f (Plus x y)              = Plus <$> f x <*> f y
  uniplate1 f (Neg x)                 = Neg <$> f x
  uniplate1 f (Mul x y)               = Mul <$> f x <*> f y
  uniplate1 f (Abs x)                 = Abs <$> f x
  uniplate1 f (Mod x y)               = Mod <$> f x <*> f y
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
  uniplate1 f (BvNot x)               = BvNot <$> f x
  uniplate1 f (BvAnd x y)             = BvAnd <$> f x <*> f y
  uniplate1 f (BvOr x y)              = BvOr <$> f x <*> f y
  uniplate1 f (BvXor x y)             = BvXor <$> f x <*> f y
  uniplate1 f (BvNand x y)            = BvNand <$> f x <*> f y
  uniplate1 f (BvNor x y)             = BvNor <$> f x <*> f y
  uniplate1 f (BvNeg x)               = BvNeg <$> f x
  uniplate1 f (BvAdd x y)             = BvAdd <$> f x <*> f y
  uniplate1 f (BvSub x y)             = BvSub <$> f x <*> f y
  uniplate1 f (BvMul x y)             = BvMul <$> f x <*> f y
  uniplate1 f (BvuDiv x y)            = BvuDiv <$> f x <*> f y
  uniplate1 f (BvuRem x y)            = BvuRem <$> f x <*> f y
  uniplate1 f (BvShL x y)             = BvShL <$> f x <*> f y
  uniplate1 f (BvLShR x y)            = BvLShR <$> f x <*> f y
  uniplate1 f (BvConcat x y)          = BvConcat <$> f x <*> f y
  uniplate1 f (BvRotL i x)            = BvRotL i <$> f x
  uniplate1 f (BvRotR i x)            = BvRotR i <$> f x
  uniplate1 f (BvuLT x y)             = BvuLT <$> f x <*> f y
  uniplate1 f (BvuLTHE x y)           = BvuLTHE <$> f x <*> f y
  uniplate1 f (BvuGTHE x y)           = BvuGTHE <$> f x <*> f y
  uniplate1 f (BvuGT x y)             = BvuGT <$> f x <*> f y
  uniplate1 f (ArrSelect i arr)       = ArrSelect i <$> f arr
  uniplate1 f (ArrStore i x arr)      = ArrStore i <$> f x <*> f arr
  uniplate1 f (StrConcat x y)         = StrConcat <$> f x <*> f y
  uniplate1 f (StrLength x)           = StrLength <$> f x
  uniplate1 f (StrLT x y)             = StrLT <$> f x <*> f y
  uniplate1 f (StrLTHE x y)           = StrLTHE <$> f x <*> f y
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
          Neg x                -> Neg  <$> tryPlate f' x
          Mul x y              -> Mul  <$> tryPlate f' x <*> tryPlate f' y
          Abs x                -> Abs  <$> tryPlate f' x
          Mod x y              -> Mod  <$> tryPlate f' x <*> tryPlate f' y
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
          BvNot x              -> BvNot  <$> tryPlate f' x
          BvAnd x y            -> BvAnd  <$> tryPlate f' x <*> tryPlate f' y
          BvOr x y             -> BvOr   <$> tryPlate f' x <*> tryPlate f' y
          BvXor x y            -> BvXor  <$> tryPlate f' x <*> tryPlate f' y
          BvNand x y           -> BvNand <$> tryPlate f' x <*> tryPlate f' y
          BvNor x y            -> BvNor  <$> tryPlate f' x <*> tryPlate f' y
          BvNeg x              -> BvNeg  <$> tryPlate f' x
          BvAdd x y            -> BvAdd  <$> tryPlate f' x <*> tryPlate f' y
          BvSub x y            -> BvSub  <$> tryPlate f' x <*> tryPlate f' y
          BvMul x y            -> BvMul  <$> tryPlate f' x <*> tryPlate f' y
          BvuDiv x y           -> BvuDiv <$> tryPlate f' x <*> tryPlate f' y
          BvuRem x y           -> BvuRem <$> tryPlate f' x <*> tryPlate f' y
          BvShL x y            -> BvShL  <$> tryPlate f' x <*> tryPlate f' y
          BvLShR x y           -> BvLShR <$> tryPlate f' x <*> tryPlate f' y
          BvConcat x y         -> BvConcat <$> tryPlate f' x <*> tryPlate f' y
          BvRotL i x           -> BvRotL i <$> tryPlate f' x
          BvRotR i x           -> BvRotR i <$> tryPlate f' x
          BvuLT x y            -> BvuLT    <$> tryPlate f' x <*> tryPlate f' y
          BvuLTHE x y          -> BvuLTHE  <$> tryPlate f' x <*> tryPlate f' y
          BvuGTHE x y          -> BvuGTHE  <$> tryPlate f' x <*> tryPlate f' y
          BvuGT x y            -> BvuGT    <$> tryPlate f' x <*> tryPlate f' y
          ArrSelect i arr      -> ArrSelect i   <$> tryPlate f' arr
          ArrStore i x arr     -> ArrStore i    <$> tryPlate f' x <*> tryPlate f' arr
          StrConcat x y        -> StrConcat     <$> tryPlate f' x <*> tryPlate f' y
          StrLength x          -> StrLength     <$> tryPlate f' x
          StrLT x y            -> StrLT         <$> tryPlate f' x <*> tryPlate f' y
          StrLTHE x y          -> StrLTHE       <$> tryPlate f' x <*> tryPlate f' y
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
    Neg e                -> grnf e
    Mul e1 e2            -> grnf e1 `seq` grnf e2
    Abs e                -> grnf e
    Mod e1 e2            -> grnf e1 `seq` grnf e2
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
    BvNot e              -> grnf e
    BvAnd e1 e2          -> grnf e1 `seq` grnf e2
    BvOr e1 e2           -> grnf e1 `seq` grnf e2
    BvXor e1 e2          -> grnf e1 `seq` grnf e2
    BvNand e1 e2         -> grnf e1 `seq` grnf e2
    BvNor e1 e2          -> grnf e1 `seq` grnf e2
    BvNeg e              -> grnf e
    BvAdd e1 e2          -> grnf e1 `seq` grnf e2
    BvSub e1 e2          -> grnf e1 `seq` grnf e2
    BvMul e1 e2          -> grnf e1 `seq` grnf e2
    BvuDiv e1 e2         -> grnf e1 `seq` grnf e2
    BvuRem e1 e2         -> grnf e1 `seq` grnf e2
    BvShL e1 e2          -> grnf e1 `seq` grnf e2
    BvLShR e1 e2         -> grnf e1 `seq` grnf e2
    BvConcat e1 e2       -> grnf e1 `seq` grnf e2
    BvRotL _ e           -> grnf e
    BvRotR _ e           -> grnf e
    BvuLT e1 e2          -> grnf e1 `seq` grnf e2
    BvuLTHE e1 e2        -> grnf e1 `seq` grnf e2
    BvuGTHE e1 e2        -> grnf e1 `seq` grnf e2
    BvuGT e1 e2          -> grnf e1 `seq` grnf e2
    ArrSelect e1 e2      -> grnf e1 `seq` grnf e2
    ArrStore e1 e2 e3    -> grnf e1 `seq` grnf e2 `seq` grnf e3
    StrConcat e1 e2      -> grnf e1 `seq` grnf e2
    StrLength e          -> grnf e
    StrLT e1 e2          -> grnf e1 `seq` grnf e2
    StrLTHE e1 e2        -> grnf e1 `seq` grnf e2
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
