{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Language.Hasmtlib.Type.SMTSort where

import Language.Hasmtlib.Internal.Constraint
import Language.Hasmtlib.Internal.Bitvec
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Type.ArrayMap
import Data.GADT.Compare
import Data.Kind
import Data.Proxy
import Data.ByteString.Builder
import qualified Data.Text as Text
import Control.Lens
import GHC.TypeLits

-- | Sorts in SMTLib2 - used as promoted type (data-kind).
data SMTSort =
    BoolSort                      -- ^ Sort of Bool
  | IntSort                       -- ^ Sort of Int
  | RealSort                      -- ^ Sort of Real
  | BvSort Nat                    -- ^ Sort of BitVec with length n
  | ArraySort SMTSort SMTSort     -- ^ Sort of Array with indices k and values v
  | StringSort                    -- ^ Sort of String

-- | Injective type-family that computes the Haskell 'Type' of an 'SMTSort'.
type family HaskellType (t :: SMTSort) = (r :: Type) | r -> t where
  HaskellType IntSort         = Integer
  HaskellType RealSort        = Double
  HaskellType BoolSort        = Bool
  HaskellType (BvSort n)      = Bitvec n
  HaskellType (ArraySort k v) = ConstArray (HaskellType k) (HaskellType v)
  HaskellType StringSort      = Text.Text

-- | Singleton for 'SMTSort'.
data SSMTSort (t :: SMTSort) where
  SIntSort    :: SSMTSort IntSort
  SRealSort   :: SSMTSort RealSort
  SBoolSort   :: SSMTSort BoolSort
  SBvSort     :: KnownNat n => Proxy n -> SSMTSort (BvSort n)
  SArraySort  :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Ord (HaskellType v)) => Proxy k -> Proxy v -> SSMTSort (ArraySort k v)
  SStringSort :: SSMTSort StringSort

deriving instance Show (SSMTSort t)
deriving instance Eq   (SSMTSort t)
deriving instance Ord  (SSMTSort t)

instance GEq SSMTSort where
  geq SIntSort SIntSort       = Just Refl
  geq SRealSort SRealSort     = Just Refl
  geq SBoolSort SBoolSort     = Just Refl
  geq (SBvSort n) (SBvSort m) = case sameNat n m of
    Nothing   -> Nothing
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
  gcompare (SBvSort n) (SBvSort m) = case cmpNat n m of
    LTI -> GLT
    EQI -> GEQ
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
instance KnownNat n => KnownSMTSort (BvSort n) where sortSing = SBvSort (Proxy @n)
instance (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k), Ord (HaskellType v)) => KnownSMTSort (ArraySort k v) where
   sortSing = SArraySort (Proxy @k) (Proxy @v)
instance KnownSMTSort StringSort                 where sortSing = SStringSort

-- | Wrapper for 'sortSing' which takes a 'Proxy'
sortSing' :: forall prxy t. KnownSMTSort t => prxy t -> SSMTSort t
sortSing' _ = sortSing @t

-- | An existential wrapper that hides some 'SMTSort' and a list of 'Constraint's holding for it.
data SomeSMTSort cs f where
  SomeSMTSort :: forall cs f (t :: SMTSort). AllC cs t => f t -> SomeSMTSort cs f

deriving instance (forall t. Show (f t)) => Show (SomeSMTSort cs f)

-- | An existential wrapper that hides some known 'SMTSort'.
type SomeKnownSMTSort f = SomeSMTSort '[KnownSMTSort] f

instance Render (SSMTSort t) where
  render SBoolSort   = "Bool"
  render SIntSort    = "Int"
  render SRealSort   = "Real"
  render (SBvSort p) = renderBinary "_" ("BitVec" :: Builder) (natVal p)
  render (SArraySort k v) = renderBinary "Array" (sortSing' k) (sortSing' v)
  render SStringSort   = "String"
  {-# INLINEABLE render #-}
