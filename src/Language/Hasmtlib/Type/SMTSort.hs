{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Language.Hasmtlib.Type.SMTSort where

import Language.Hasmtlib.Internal.Bitvec
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Type.ArrayMap
import Data.GADT.Compare
import Data.Kind
import Data.Proxy
import Data.ByteString.Builder
import Control.Lens
import GHC.TypeLits

-- | Sorts in SMTLib2 - used as promoted type (data-kind).
data SMTSort =
    BoolSort                      -- ^ Sort of Bool
  | IntSort                       -- ^ Sort of Int
  | RealSort                      -- ^ Sort of Real
  | BvSort Nat                    -- ^ Sort of BitVec with length n
  | ArraySort SMTSort SMTSort     -- ^ Sort of Array with indices k and values v

-- | Injective type-family that computes the Haskell 'Type' of an 'SMTSort'.
type family HaskellType (t :: SMTSort) = (r :: Type) | r -> t where
  HaskellType IntSort         = Integer
  HaskellType RealSort        = Double
  HaskellType BoolSort        = Bool
  HaskellType (BvSort n)      = Bitvec n
  HaskellType (ArraySort k v) = ConstArray (HaskellType k) (HaskellType v)

-- | Singleton for 'SMTSort'.
data SSMTSort (t :: SMTSort) where
  SIntSort   :: SSMTSort IntSort
  SRealSort  :: SSMTSort RealSort
  SBoolSort  :: SSMTSort BoolSort
  SBvSort    :: KnownNat n => Proxy n -> SSMTSort (BvSort n)
  SArraySort :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Proxy k -> Proxy v -> SSMTSort (ArraySort k v)

deriving instance Show (SSMTSort t)
deriving instance Eq   (SSMTSort t)
deriving instance Ord  (SSMTSort t)

instance GEq SSMTSort where
  geq SIntSort SIntSort       = Just Refl
  geq SRealSort SRealSort     = Just Refl
  geq SBoolSort SBoolSort     = Just Refl
  geq (SBvSort n) (SBvSort m) = case sameNat n m of
    Just Refl -> Just Refl
    Nothing   -> Nothing
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
  gcompare SBoolSort _        = GLT
  gcompare _ SBoolSort        = GGT
  gcompare SIntSort _         = GLT
  gcompare _ SIntSort         = GGT
  gcompare SRealSort _        = GLT
  gcompare _ SRealSort        = GGT
  gcompare (SArraySort _ _) _ = GLT
  gcompare _ (SArraySort _ _) = GGT

-- | Compute singleton 'SSMTSort' from it's promoted type 'SMTSort'.
class    KnownSMTSort (t :: SMTSort)           where sortSing :: SSMTSort t
instance KnownSMTSort IntSort                  where sortSing = SIntSort
instance KnownSMTSort RealSort                 where sortSing = SRealSort
instance KnownSMTSort BoolSort                 where sortSing = SBoolSort
instance KnownNat n => KnownSMTSort (BvSort n) where sortSing = SBvSort (Proxy @n)
instance (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => KnownSMTSort (ArraySort k v) where
   sortSing = SArraySort (Proxy @k) (Proxy @v)

-- | Wrapper for 'sortSing' which takes a 'Proxy'-like argument for @t@.
sortSing' :: forall prxy t. KnownSMTSort t => prxy t -> SSMTSort t
sortSing' _ = sortSing @t

-- | AllC ensures that a list of constraints is applied to a poly-kinded 'Type' k
--
-- @
-- AllC '[]       k = ()
-- AllC (c ': cs) k = (c k, AllC cs k)
-- @
type AllC :: [k -> Constraint] -> k -> Constraint
type family AllC cs k :: Constraint where
  AllC '[]       k = ()
  AllC (c ': cs) k = (c k, AllC cs k)

-- | An existential wrapper that hides some 'SMTSort' and a list of 'Constraint's holding for it.
data SomeSMTSort cs f where
  SomeSMTSort :: forall cs f (t :: SMTSort). AllC cs t => f t -> SomeSMTSort cs f

deriving instance (forall t. Show (f t)) => Show (SomeSMTSort cs f)

instance (forall t. Eq (f t)) => Eq (SomeSMTSort (KnownSMTSort ': cs) f) where
  (SomeSMTSort x) == (SomeSMTSort y) = case geq (sortSing' x) (sortSing' y) of
    Nothing   -> False
    Just Refl -> x == y

instance (forall t. Ord (f t)) => Ord (SomeSMTSort (KnownSMTSort ': cs) f) where
  compare (SomeSMTSort x) (SomeSMTSort y) = case gcompare (sortSing' x) (sortSing' y) of
    GLT -> LT
    GEQ -> compare x y
    GGT -> GT

instance Render (SSMTSort t) where
  render SBoolSort   = "Bool"
  render SIntSort    = "Int"
  render SRealSort   = "Real"
  render (SBvSort p) = renderBinary "_" ("BitVec" :: Builder) (natVal p)
  render (SArraySort k v) = renderBinary "Array" (sortSing' k) (sortSing' v)
  {-# INLINEABLE render #-}
