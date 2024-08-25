{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Hasmtlib.Internal.Bitvec where

import Prelude hiding ((&&), (||), not)
import Language.Hasmtlib.Boolean
import Language.Hasmtlib.Internal.Render
import Data.ByteString.Builder
import Data.Bit
import Data.Bits
import Data.Coerce
import Data.Finite hiding (shift)
import Data.Proxy
import Data.Ratio ((%))
import Data.Bifunctor
import qualified Data.Vector.Unboxed.Sized as V
import GHC.TypeNats

-- | Unsigned and length-indexed bitvector with MSB first.
newtype Bitvec (n :: Nat) = Bitvec { unBitvec :: V.Vector n Bit }
  deriving newtype (Eq, Ord, Boolean)

instance KnownNat n => Bits (Bitvec n) where
  (.&.) = (&&)
  (.|.) = (||)
  xor = Language.Hasmtlib.Boolean.xor
  complement = not
  shift bv i  = coerce $ shift (coerce @_ @(V.Vector n Bit) bv) (negate i)
  rotate bv i = coerce $ rotate (coerce @_ @(V.Vector n Bit) bv) (negate i)
  bitSize _ = fromIntegral $ natVal (Proxy @n)
  bitSizeMaybe _ = Just $ fromIntegral $ natVal (Proxy @n)
  isSigned _ = false
  testBit bv = testBit (V.reverse (coerce @_ @(V.Vector n Bit) bv))
  bit (toInteger -> i) = coerce $ V.reverse $ V.replicate @n (Bit False) V.// [(finite i, Bit True)]
  popCount = coerce . popCount . coerce @_ @(V.Vector n Bit)

instance Show (Bitvec n) where
  show = V.toList . V.map (\b -> if coerce b then '1' else '0') . coerce @_ @(V.Vector n Bit)

instance Render (Bitvec n) where
  render = stringUtf8 . show
  {-# INLINEABLE render #-}

instance KnownNat n => Num (Bitvec n) where
   fromInteger x = coerce . V.reverse $ V.generate @n (coerce . testBit x . fromInteger . getFinite)
   negate        = id
   abs           = id
   signum _      = 0
   (coerce -> x) + (coerce -> y) = coerce @(V.Vector n Bit) $ x + y
   (coerce -> x) - (coerce -> y) = coerce @(V.Vector n Bit) $ x - y
   (coerce -> x) * (coerce -> y) = coerce @(V.Vector n Bit) $ x * y

instance KnownNat n => Bounded (Bitvec n) where
  minBound = coerce $ V.replicate @n false
  maxBound = coerce $ V.replicate @n true

instance KnownNat n => Enum (Bitvec n) where
  succ x   = x + 1
  pred x   = x - 1
  toEnum   = fromInteger . toInteger
  fromEnum = V.sum . V.imap (\i b -> if coerce b then 2 ^ getFinite i else 0) . V.reverse . coerce @_ @(V.Vector n Bit)

instance KnownNat n => Real (Bitvec n) where
  toRational x = toInteger (fromEnum x) % 1

instance KnownNat n => Integral (Bitvec n) where
  toInteger = fromIntegral . fromEnum
  quotRem x y = bimap fromInteger fromInteger $ quotRem (toInteger x) (toInteger y)

bvReverse :: Bitvec n -> Bitvec n
bvReverse = coerce . V.reverse . coerce

bvReplicate :: forall n. KnownNat n => Bit -> Bitvec n
bvReplicate = coerce . V.replicate @n

bvReplicate' :: forall n proxy. KnownNat n => proxy n -> Bit -> Bitvec n
bvReplicate' _ = bvReplicate

bvGenerate :: forall n. KnownNat n => (Finite n -> Bit) -> Bitvec n
bvGenerate = coerce . V.generate @n . coerce

bvConcat :: Bitvec n -> Bitvec m -> Bitvec (n + m)
bvConcat (coerce -> x) (coerce -> y) = coerce $ x V.++ y

bvTake' :: forall n m proxy . KnownNat n => proxy n -> Bitvec (n+m) -> Bitvec n
bvTake' p = coerce . V.take' p . coerce

bvDrop' :: forall n m proxy . KnownNat n => proxy n -> Bitvec (n+m) -> Bitvec m
bvDrop' p = coerce . V.drop' p . coerce

bvSplitAt' :: forall n m proxy. KnownNat n => proxy n -> Bitvec (n+m) -> (Bitvec n, Bitvec m)
bvSplitAt' p = coerce . V.splitAt' p . coerce

bvToList :: Bitvec n -> [Bit]
bvToList = V.toList . coerce

bvFromListN :: forall n. KnownNat n => [Bit] -> Maybe (Bitvec n)
bvFromListN = coerce . V.fromListN @n

bvFromListN' :: forall n. KnownNat n => Proxy n -> [Bit] -> Maybe (Bitvec n)
bvFromListN' _ = bvFromListN

bvShL :: KnownNat n => Bitvec n -> Bitvec n -> Maybe (Bitvec n)
bvShL x y = bvFromListN $ (++ replicate i false) $ drop i $ bvToList x
  where
    i = fromIntegral y

bvLShR :: KnownNat n => Bitvec n -> Bitvec n -> Maybe (Bitvec n)
bvLShR x y = fmap bvReverse $ bvFromListN $ (++ replicate i false) $ drop i $ bvToList $ bvReverse x
  where
    i = fromIntegral y

bvZeroExtend :: KnownNat i => Proxy i -> Bitvec n -> Bitvec (n+i)
bvZeroExtend p x = bvConcat x $ bvReplicate' p false

bvExtract :: forall n i j.
  ( KnownNat i, KnownNat ((j - i) + 1)
  , (i+(n-i)) ~ n
  , (((j - i) + 1) + ((n - i)-((j - i) + 1))) ~ (n - i)
  ) => Proxy i -> Proxy j -> Bitvec n -> Bitvec (( j - i ) + 1)
bvExtract pri _ x = bvTake' @_ @((n-i)-((j-i)+1)) (Proxy @((j-i)+1)) x'
  where
    x' :: Bitvec (n-i) = bvDrop' pri x
