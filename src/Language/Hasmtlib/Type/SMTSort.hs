{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

{- |
This module provides the data-type 'SMTSort' and some singleton-operations for it.

The type 'SMTSort' is only used as promoted type (data-kind) in dependent-like contexts such as GADTs.
-}
module Language.Hasmtlib.Type.SMTSort
(
  -- * Type
  SMTSort(..)
, HaskellType

  -- * Singleton
, SSMTSort(..)
, KnownSMTSort(..), sortSing'

  -- * Existential
, SomeKnownSMTSort
)
where

import Language.Hasmtlib.Type.Bitvec
import Language.Hasmtlib.Type.ArrayMap
import Data.GADT.Compare
import Data.Kind
import Data.Proxy
import Data.Some.Constraint
import qualified Data.Text as Text
import Control.Lens
import GHC.TypeLits

-- | Sorts in SMTLib2 - used as promoted type (data-kind).
data SMTSort =
    BoolSort                      -- ^ Sort of Bool
  | IntSort                       -- ^ Sort of Int
  | RealSort                      -- ^ Sort of Real
  | BvSort BvEnc Nat              -- ^ Sort of BitVec with type of encoding enc and length n
  | ArraySort SMTSort SMTSort     -- ^ Sort of Array with indices k and values v
  | StringSort                    -- ^ Sort of String

-- | Injective type-family that computes the Haskell 'Type' of an 'SMTSort'.
type family HaskellType (t :: SMTSort) = (r :: Type) | r -> t where
  HaskellType IntSort         = Integer
  HaskellType RealSort        = Double
  HaskellType BoolSort        = Bool
  HaskellType (BvSort enc n)  = Bitvec enc n
  HaskellType (ArraySort k v) = ConstArray (HaskellType k) (HaskellType v)
  HaskellType StringSort      = Text.Text

-- | Singleton for 'SMTSort'.
data SSMTSort (t :: SMTSort) where
  SIntSort    :: SSMTSort IntSort
  SRealSort   :: SSMTSort RealSort
  SBoolSort   :: SSMTSort BoolSort
  SBvSort     :: (KnownBvEnc enc, KnownNat n) => Proxy enc -> Proxy n -> SSMTSort (BvSort enc n)
  SArraySort  :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Ord (HaskellType v)) => Proxy k -> Proxy v -> SSMTSort (ArraySort k v)
  SStringSort :: SSMTSort StringSort

deriving instance Show (SSMTSort t)
deriving instance Eq   (SSMTSort t)
deriving instance Ord  (SSMTSort t)

instance GEq SSMTSort where
  geq SIntSort SIntSort       = Just Refl
  geq SRealSort SRealSort     = Just Refl
  geq SBoolSort SBoolSort     = Just Refl
  geq (SBvSort enc n) (SBvSort emc m) = case sameNat n m of
    Nothing   -> Nothing
    Just Refl -> case geq (bvEncSing' enc) (bvEncSing' emc) of
      Nothing -> Nothing
      Just Refl -> Just Refl
  geq (SArraySort k v) (SArraySort k' v') = case geq (sortSing' k) (sortSing' k') of
    Nothing   -> Nothing
    Just Refl -> case geq (sortSing' v) (sortSing' v') of
      Nothing -> Nothing
      Just Refl -> Just Refl
  geq SStringSort SStringSort = Just Refl
  geq _ _                     = Nothing

instance GCompare SSMTSort where
  gcompare SBoolSort SBoolSort     = GEQ
  gcompare SIntSort SIntSort       = GEQ
  gcompare SRealSort SRealSort     = GEQ
  gcompare (SBvSort enc n) (SBvSort emc m) = case cmpNat n m of
    LTI -> GLT
    EQI -> case gcompare (bvEncSing' enc) (bvEncSing' emc) of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT
    GTI -> GGT
  gcompare (SArraySort k v) (SArraySort k' v') = case gcompare (sortSing' k) (sortSing' k') of
    GLT -> GLT
    GEQ -> case gcompare (sortSing' v) (sortSing' v') of
      GLT -> GLT
      GEQ -> GEQ
      GGT -> GGT
    GGT -> GGT
  gcompare SStringSort SStringSort = GEQ
  gcompare SBoolSort _        = GLT
  gcompare _ SBoolSort        = GGT
  gcompare SIntSort _         = GLT
  gcompare _ SIntSort         = GGT
  gcompare SRealSort _        = GLT
  gcompare _ SRealSort        = GGT
  gcompare (SArraySort _ _) _ = GLT
  gcompare _ (SArraySort _ _) = GGT
  gcompare SStringSort _      = GLT
  gcompare _ SStringSort      = GGT

-- | Compute singleton 'SSMTSort' from it's promoted type 'SMTSort'.
class    KnownSMTSort (t :: SMTSort)           where sortSing :: SSMTSort t
instance KnownSMTSort IntSort                  where sortSing = SIntSort
instance KnownSMTSort RealSort                 where sortSing = SRealSort
instance KnownSMTSort BoolSort                 where sortSing = SBoolSort
instance (KnownBvEnc enc, KnownNat n) => KnownSMTSort (BvSort enc n) where sortSing = SBvSort (Proxy @enc) (Proxy @n)
instance (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Ord (HaskellType v)) => KnownSMTSort (ArraySort k v) where
   sortSing = SArraySort (Proxy @k) (Proxy @v)
instance KnownSMTSort StringSort                 where sortSing = SStringSort

-- | Wrapper for 'sortSing' which takes a 'Proxy'.
sortSing' :: forall prxy t. KnownSMTSort t => prxy t -> SSMTSort t
sortSing' _ = sortSing @t

-- | An existential wrapper that hides some known 'SMTSort'.
type SomeKnownSMTSort f = Some1 ((~) f) KnownSMTSort
