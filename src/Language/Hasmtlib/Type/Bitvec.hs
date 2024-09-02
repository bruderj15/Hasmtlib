{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RoleAnnotations #-}

{- |
This module provides the type-level length-indexed and MSB-first bitvector 'Bitvec'
built on top of the package @bitvec@ and 'V.Vector'.

It also holds it's type of encoding as a phantom-type via 'BvEnc'.

==== __Examples__

>>> minBound @(Bitvec Unsigned 8)
00000000

>>> maxBound @(Bitvec Signed 8)
01111111

>>> (5 :: Bitvec Unsigned 4) + 10
1111
-}
module Language.Hasmtlib.Type.Bitvec
(
  -- * Bitvector encoding
  BvEnc(..), SBvEnc(..), KnownBvEnc(..)
, bvEncSing', bvEncSing''

  -- * Bitvec type
, Bitvec(..)

  -- * Construction
, bitvecConcat

  -- * Conversion
  -- ** Sign
, asUnsigned, asSigned

  -- ** Lists
, bitvecFromListN, bitvecFromListN'

  -- * Lens
, _signBit
)
where

import Prelude hiding ((&&), (||), not)
import Language.Hasmtlib.Boolean
import Language.Hasmtlib.Internal.Render
import Data.GADT.Compare
import Data.ByteString.Builder
import Data.Bit
import Data.Bits
import Data.Coerce
import Data.Finite hiding (shift)
import Data.Proxy
import Data.Bifunctor
import Data.Type.Equality
import qualified Data.Vector.Unboxed.Sized as V
import Control.Lens
import GHC.TypeNats

-- | Type of Bitvector encoding - used as promoted type (data-kind).
data BvEnc = Unsigned | Signed deriving (Show, Eq, Ord)

-- | Singleton for 'BvEnc'.
data SBvEnc (enc :: BvEnc) where
  SUnsigned :: SBvEnc Unsigned
  SSigned   :: SBvEnc Signed

deriving instance Show (SBvEnc enc)
deriving instance Eq   (SBvEnc enc)
deriving instance Ord  (SBvEnc enc)

-- | Compute singleton 'SBvEnc' from it's promoted type 'BvEnc'.
class KnownBvEnc (enc :: BvEnc) where bvEncSing :: SBvEnc enc
instance KnownBvEnc Unsigned    where bvEncSing = SUnsigned
instance KnownBvEnc Signed      where bvEncSing = SSigned

-- | Wrapper for 'bvEncSing' which takes a 'Proxy'.
bvEncSing' :: forall enc prxy. KnownBvEnc enc => prxy enc -> SBvEnc enc
bvEncSing' _ = bvEncSing @enc

-- | Wrapper for 'bvEncSing' which takes a 'Proxy' and some ballast.
--   This is helpful for singing on values of type 'Bitvec' where the ballst is a 'Nat'.
bvEncSing'' :: forall enc a prxy. KnownBvEnc enc => prxy enc a -> SBvEnc enc
bvEncSing'' _ = bvEncSing @enc

instance GEq SBvEnc where
  geq SUnsigned SUnsigned = Just Refl
  geq SSigned SSigned = Just Refl
  geq _ _ = Nothing

instance GCompare SBvEnc where
  gcompare SUnsigned SUnsigned = GEQ
  gcompare SUnsigned _  = GLT
  gcompare _ SUnsigned  = GGT
  gcompare SSigned SSigned = GEQ
  -- gcompare SSigned _  = GLT
  -- gcompare _ SSigned  = GGT

-- | Length-indexed bitvector ('V.Vector') carrying a phantom type-level 'BvEnc'.
--
--   Most significant bit is first (index 0) for unsigned bitvectors.
--   Signed bitvectors have their sign bit first (index 0) and their most significant bit second (index 1).
type role Bitvec phantom phantom
newtype Bitvec (enc :: BvEnc) (n :: Nat) = Bitvec { unBitvec :: V.Vector n Bit }
  deriving newtype (Eq, Ord, Boolean)

-- | Convert 'Bitvec' with any encoding 'BvEnc' to 'Unsigned'.
asUnsigned :: forall enc n. Bitvec enc n -> Bitvec Unsigned n
asUnsigned = coerce . coerce @(Bitvec enc n) @(V.Vector n Bit)

-- | Convert 'Bitvec' with any encoding 'BvEnc' to 'Signed'.
asSigned :: forall enc n. Bitvec enc n -> Bitvec Signed n
asSigned = coerce . coerce @(Bitvec enc n) @(V.Vector n Bit)

instance Show (Bitvec enc n) where
  show = V.toList . V.map (\b -> if coerce b then '1' else '0') . coerce @_ @(V.Vector n Bit)
  {-# INLINEABLE show #-}

instance Render (Bitvec enc n) where
  render = stringUtf8 . show
  {-# INLINE render #-}

instance (KnownBvEnc enc, KnownNat n) => Bits (Bitvec enc n) where
  (.&.) = (&&)
  (.|.) = (||)
  xor = Language.Hasmtlib.Boolean.xor
  complement = not
  shift bv i  = coerce $ shift (coerce @_ @(V.Vector n Bit) bv) (negate i)
  rotate bv i = coerce $ rotate (coerce @_ @(V.Vector n Bit) bv) (negate i)
  bitSize _ = fromIntegral $ natVal (Proxy @n)
  bitSizeMaybe _ = Just $ fromIntegral $ natVal (Proxy @n)
  isSigned _ = case bvEncSing @enc of
    SUnsigned -> False
    SSigned -> True
  testBit bv = testBit (V.reverse (coerce @_ @(V.Vector n Bit) bv))
  bit (toInteger -> i) = coerce $ V.reverse $ V.replicate @n (Bit False) V.// [(finite i, Bit True)]
  popCount = coerce . popCount . coerce @_ @(V.Vector n Bit)

instance (KnownBvEnc enc, KnownNat n) => Num (Bitvec enc n) where
   fromInteger x = coerce . V.reverse $ V.generate @n (coerce . testBit x . fromInteger . getFinite)
   negate        = case bvEncSing @enc of
    SUnsigned -> id
    SSigned -> (+1) . not
   abs x         = if signum x < 0 then negate x else x
   signum x      = case bvEncSing @enc of
    SUnsigned -> 0
    SSigned -> if testBit x 0 then -1 else 1
   x + y = fromInteger $ toInteger x + toInteger y
   x - y = fromInteger $ toInteger x - toInteger y
   x * y = fromInteger $ toInteger x * toInteger y

instance (KnownBvEnc enc, KnownNat n) => Bounded (Bitvec enc n) where
  minBound = case bvEncSing @enc of
    SUnsigned -> coerce $ V.replicate @n false
    SSigned -> coerce $ setBit (V.replicate @n false) 0
  maxBound = case bvEncSing @enc of
    SUnsigned -> coerce $ V.replicate @n true
    SSigned -> coerce $ clearBit (V.replicate @n true) 0

instance (KnownBvEnc enc, KnownNat n) => Enum (Bitvec enc n) where
  toEnum     = fromInteger . toInteger
  fromEnum x = case bvEncSing @enc of
    SUnsigned -> V.sum . V.imap (\i b -> if coerce b then 2 ^ getFinite i else 0) . V.reverse $ coerce @_ @(V.Vector n Bit) x
    SSigned -> if testBit x 0
          then negate . (+1) . V.sum . V.imap (\i b -> if coerce b then 2 ^ getFinite i else 0) . V.reverse $ coerce @_ @(V.Vector n Bit) $ not x
          else V.sum . V.imap (\i b -> if coerce b then 2 ^ getFinite i else 0) . V.reverse $ coerce @_ @(V.Vector n Bit) x

instance (KnownBvEnc enc, KnownNat n) => Real (Bitvec enc n) where
  toRational = toRational . fromEnum

instance (KnownBvEnc enc, KnownNat n) => Integral (Bitvec enc n) where
  toInteger = fromIntegral . fromEnum
  quotRem x y = bimap fromInteger fromInteger $ quotRem (toInteger x) (toInteger y)

-- | A Lens on the sign-bit of a signed bitvector.
_signBit :: KnownNat (n + 1) => Lens' (Bitvec Signed (n + 1)) Bit
_signBit = lens (\bv -> Bit $ testBit bv 0 )$
  \bv b -> case b of
    Bit False -> clearBit bv 0
    Bit True  -> setBit   bv 0

-- | Concatenation of two bitvectors.
bitvecConcat :: Bitvec enc n -> Bitvec enc m -> Bitvec enc (n + m)
bitvecConcat (coerce -> x) (coerce -> y) = coerce $ x V.++ y

-- | Constructing a bitvector from a list.
bitvecFromListN :: forall n enc. KnownNat n => [Bit] -> Maybe (Bitvec enc n)
bitvecFromListN = coerce . V.fromListN @n

-- | Constructing a bitvector from a list with length given as 'Proxy'.
bitvecFromListN' :: KnownNat n => Proxy n -> [Bit] -> Maybe (Bitvec enc n)
bitvecFromListN' _ = bitvecFromListN
