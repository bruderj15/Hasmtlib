{-# LANGUAGE UndecidableInstances #-}

{- |
This module provides the wrapper 'Value' for embedding Haskell-Values into the SMT-Context.

The Direction @Haskell-Value => SMT-Value@ is the creation of a constant in the SMT-Problem.

The other way around @SMT-Value => Haskell-Value@ is mainly used when decoding a solvers solutions for variables.
-}
module Language.Hasmtlib.Type.Value
(
  -- * Type
  Value(..)

  -- * Conversion
, wrapValue, unwrapValue
)
where

import Prelude hiding (not, (&&), (||))
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Bitvec
import Language.Hasmtlib.Boolean
import Data.GADT.Compare
import Data.Proxy
import Control.Lens
import GHC.TypeLits

-- | A wrapper for values of 'SMTSort's.
--   This wraps a Haskell-value into the SMT-Context.
data Value (t :: SMTSort) where
  IntValue    :: HaskellType IntSort    -> Value IntSort
  RealValue   :: HaskellType RealSort   -> Value RealSort
  BoolValue   :: HaskellType BoolSort   -> Value BoolSort
  BvValue     :: (KnownBvEnc enc, KnownNat n) => HaskellType (BvSort enc n) -> Value (BvSort enc n)
  ArrayValue  :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Ord (HaskellType v)) => HaskellType (ArraySort k v) -> Value (ArraySort k v)
  StringValue :: HaskellType StringSort -> Value StringSort

deriving instance Eq (HaskellType t) => Eq (Value t)
deriving instance Ord (HaskellType t) => Ord (Value t)

instance GEq Value where
  geq (BoolValue x) (BoolValue y)   = if x == y then Just Refl else Nothing
  geq (IntValue x) (IntValue y)     = if x == y then Just Refl else Nothing
  geq (RealValue x) (RealValue y)   = if x == y then Just Refl else Nothing
  geq (BvValue x) (BvValue y)       = case cmpNat x y of
    EQI -> case geq (bvEncSing'' x) (bvEncSing'' y) of
      Nothing -> Nothing
      Just Refl -> if x == y then Just Refl else Nothing
    _   -> Nothing
  geq ax@(ArrayValue x) ay@(ArrayValue y) = case geq (sortSing' ax) (sortSing' ay) of
    Nothing -> Nothing
    Just Refl -> if x == y then Just Refl else Nothing
  geq (StringValue x) (StringValue y) = if x == y then Just Refl else Nothing
  geq _ _ = Nothing

liftOrdering :: forall {k} {a :: k}. Ordering -> GOrdering a a
liftOrdering LT = GLT
liftOrdering EQ = GEQ
liftOrdering GT = GGT
{-# INLINE liftOrdering #-}

instance GCompare Value where
  gcompare (BoolValue x) (BoolValue x')     = liftOrdering $ compare x x'
  gcompare (IntValue x)  (IntValue x')      = liftOrdering $ compare x x'
  gcompare (RealValue x) (RealValue x')     = liftOrdering $ compare x x'
  gcompare (BvValue x) (BvValue x')         = case cmpNat x x' of
    LTI -> GLT
    EQI -> case gcompare (bvEncSing'' x) (bvEncSing'' x') of
      GLT -> GLT
      GEQ -> liftOrdering $ compare x x'
      GGT -> GGT
    GTI -> GGT
  gcompare (ArrayValue x) (ArrayValue x')   = case gcompare (sortSing' (pk x)) (sortSing' (pk x')) of
    GLT -> GLT
    GEQ -> case gcompare (sortSing' (pv x)) (sortSing' (pv x')) of
      GLT -> GLT
      GEQ -> liftOrdering $ compare x x'
      GGT -> GGT
    GGT -> GGT
    where
      pk :: forall k v. HaskellType (ArraySort k v) -> Proxy k
      pk _ = Proxy @k
      pv :: forall k v. HaskellType (ArraySort k v) -> Proxy v
      pv _ = Proxy @v
  gcompare (StringValue x)(StringValue x')  = liftOrdering $ compare x x'
  gcompare (BoolValue _) _                  = GLT
  gcompare _ (BoolValue _)                  = GGT
  gcompare (IntValue _) _                   = GLT
  gcompare _ (IntValue _)                   = GGT
  gcompare (RealValue _) _                  = GLT
  gcompare _ (RealValue _)                  = GGT
  gcompare (BvValue _) _                    = GLT
  gcompare _ (BvValue _)                    = GGT
  gcompare (ArrayValue _) _                 = GLT
  gcompare _ (ArrayValue _)                 = GGT
  -- gcompare (StringValue _) _                = GLT
  -- gcompare _ (StringValue _)                = GGT

instance (KnownSMTSort t, Num (HaskellType t)) => Num (Value t) where
  fromInteger = wrapValue . fromInteger
  {-# INLINE fromInteger #-}
  x + y = wrapValue $ unwrapValue x + unwrapValue y
  {-# INLINE (+) #-}
  x - y = wrapValue $ unwrapValue x - unwrapValue y
  {-# INLINE (-) #-}
  x * y = wrapValue $ unwrapValue x * unwrapValue y
  {-# INLINE (*) #-}
  negate = wrapValue . negate . unwrapValue
  {-# INLINE negate #-}
  abs = wrapValue . abs . unwrapValue
  {-# INLINE abs #-}
  signum = wrapValue . signum . unwrapValue
  {-# INLINE signum #-}

instance Fractional (Value RealSort) where
  fromRational = RealValue . fromRational
  {-# INLINE fromRational #-}
  (RealValue x) / (RealValue y) = RealValue $ x / y
  {-# INLINE (/) #-}

instance Boolean (Value BoolSort) where
  bool = BoolValue
  {-# INLINE bool #-}
  (BoolValue x) && (BoolValue y) = BoolValue $ x && y
  {-# INLINE (&&) #-}
  (BoolValue x) || (BoolValue y) = BoolValue $ x || y
  {-# INLINE (||) #-}
  not (BoolValue x) = BoolValue $ not x
  {-# INLINE not #-}
  xor (BoolValue x) (BoolValue y) = BoolValue $ x `xor` y
  {-# INLINE xor #-}

-- | Unwraps a Haskell-value from the SMT-Context-'Value'.
unwrapValue :: Value t -> HaskellType t
unwrapValue (IntValue  v)   = v
unwrapValue (RealValue v)   = v
unwrapValue (BoolValue v)   = v
unwrapValue (BvValue   v)   = v
unwrapValue (ArrayValue v)  = v
unwrapValue (StringValue v) = v
{-# INLINE unwrapValue #-}

-- | Wraps a Haskell-value into the SMT-Context-'Value'.
wrapValue :: forall t. KnownSMTSort t => HaskellType t -> Value t
wrapValue = case sortSing @t of
  SIntSort       -> IntValue
  SRealSort      -> RealValue
  SBoolSort      -> BoolValue
  SBvSort _ _    -> BvValue
  SArraySort _ _ -> ArrayValue
  SStringSort    -> StringValue
{-# INLINE wrapValue #-}
