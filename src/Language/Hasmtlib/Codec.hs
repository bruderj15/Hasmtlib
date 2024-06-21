{-# LANGUAGE DefaultSignatures #-}
-- required for DefaultEncoded a
{-# LANGUAGE UndecidableInstances #-}  

module Language.Hasmtlib.Codec where

import Prelude hiding (not, (&&), (||))
import Language.Hasmtlib.Internal.Bitvec
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Type.ArrayMap
import Language.Hasmtlib.Boolean
import Data.Kind
import Data.Coerce
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.IntMap as IM
import Data.Dependent.Map as DMap
import Data.Tree (Tree)
import Control.Monad

-- | Compute the default 'Decoded' 'Type' for every functor-wrapper.
--   Useful for instances using default signatures.
type family DefaultDecoded a :: Type where
  DefaultDecoded (f a) = f (Decoded a)

-- | Lift values to SMT-Values or decode them.
class Codec a where
  type Decoded a :: Type
  type Decoded a = DefaultDecoded a

  -- | Decode a value using given solution.
  decode :: Solution -> a -> Maybe (Decoded a)
  default decode :: (Traversable f, Codec b, a ~ f b, Decoded a ~ f (Decoded b)) => Solution -> a -> Maybe (Decoded a)
  decode sol = traverse (decode sol)

  -- | Encode a value as constant.
  encode :: Decoded a -> a
  default encode :: (Functor f, Codec b, a ~ f b, Decoded a ~ f (Decoded b)) => Decoded a -> a
  encode = fmap encode

-- | Decode and evaluate expressions
instance KnownSMTSort t => Codec (Expr t) where
  type Decoded (Expr t) = HaskellType t
  decode sol (Var var)  = do
    (IntValueMap m) <- DMap.lookup (sortSing @t) sol
    val <- IM.lookup (coerce var) m
    return $ unwrapValue val
  decode _ (Constant v)         = Just $ unwrapValue v
  decode sol (Plus x y)         = liftA2 (+)   (decode sol x) (decode sol y)
  decode sol (Neg x)            = fmap negate  (decode sol x)
  decode sol (Mul x y)          = liftA2 (*)   (decode sol x) (decode sol y)
  decode sol (Abs x)            = fmap abs     (decode sol x)
  decode sol (Mod x y)          = liftA2 mod   (decode sol x) (decode sol y)
  decode sol (IDiv x y)         = liftA2 div   (decode sol x) (decode sol y)
  decode sol (Div x y)          = liftA2 (/)   (decode sol x) (decode sol y)
  decode sol (LTH x y)          = liftA2 (<)   (decode sol x) (decode sol y)
  decode sol (LTHE x y)         = liftA2 (<=)  (decode sol x) (decode sol y)
  decode sol (EQU x y)          = liftA2 (==)  (decode sol x) (decode sol y)
  decode sol (Distinct x y)     = liftA2 (/=)  (decode sol x) (decode sol y)
  decode sol (GTHE x y)         = liftA2 (>=)  (decode sol x) (decode sol y)
  decode sol (GTH x y)          = liftA2 (>)   (decode sol x) (decode sol y)
  decode sol (Not x)            = fmap   not  (decode sol x)
  decode sol (And x y)          = liftA2 (&&) (decode sol x) (decode sol y)
  decode sol (Or x y)           = liftA2 (||) (decode sol x) (decode sol y)
  decode sol (Impl x y)         = liftA2 (==>) (decode sol x) (decode sol y)
  decode sol (Xor x y)          = liftA2 xor   (decode sol x) (decode sol y)
  decode _ Pi                   = Just pi
  decode sol (Sqrt x)           = fmap sqrt  (decode sol x)
  decode sol (Exp x)            = fmap exp   (decode sol x)
  decode sol (Sin x)            = fmap sin   (decode sol x)
  decode sol (Cos x)            = fmap cos   (decode sol x)
  decode sol (Tan x)            = fmap tan   (decode sol x)
  decode sol (Asin x)           = fmap asin  (decode sol x)
  decode sol (Acos x)           = fmap acos  (decode sol x)
  decode sol (Atan x)           = fmap atan  (decode sol x)
  decode sol (ToReal x)         = fmap realToFrac (decode sol x)
  decode sol (ToInt x)          = fmap truncate   (decode sol x)
  decode sol (IsInt x)          = fmap ((0 ==) . snd . properFraction) (decode sol x)
  decode sol (Ite p t f)        = liftM3 (\p' t' f' -> if p' then t' else f') (decode sol p) (decode sol t) (decode sol f) 
  decode sol (BvNot x)          = fmap not (decode sol x)
  decode sol (BvAnd x y)        = liftA2 (&&) (decode sol x) (decode sol y)
  decode sol (BvOr x y)         = liftA2 (||) (decode sol x) (decode sol y)
  decode sol (BvXor x y)        = liftA2 xor (decode sol x) (decode sol y)
  decode sol (BvNand x y)       = nand <$> sequenceA [decode sol x, decode sol y]
  decode sol (BvNor x y)        = nor  <$> sequenceA [decode sol x, decode sol y]
  decode sol (BvNeg x)          = fmap negate (decode sol x)
  decode sol (BvAdd x y)        = liftA2 (+) (decode sol x) (decode sol y)
  decode sol (BvSub x y)        = liftA2 (-) (decode sol x) (decode sol y)
  decode sol (BvMul x y)        = liftA2 (*) (decode sol x) (decode sol y)
  decode sol (BvuDiv x y)       = liftA2 div (decode sol x) (decode sol y)
  decode sol (BvuRem x y)       = liftA2 rem (decode sol x) (decode sol y)
  decode sol (BvShL x y)        = join $ liftA2 bvShL (decode sol x) (decode sol y)
  decode sol (BvLShR x y)       = join $ liftA2 bvLShR (decode sol x) (decode sol y)
  decode sol (BvConcat x y)     = liftA2 bvConcat (decode sol x) (decode sol y)
  decode sol (BvRotL i x)       = bvRotL i <$> decode sol x
  decode sol (BvRotR i x)       = bvRotR i <$> decode sol x
  decode sol (BvuLT x y)        = liftA2 (<) (decode sol x) (decode sol y)
  decode sol (BvuLTHE x y)      = liftA2 (<=) (decode sol x) (decode sol y)
  decode sol (BvuGTHE x y)      = liftA2 (>=) (decode sol x) (decode sol y)
  decode sol (BvuGT x y)        = liftA2 (>) (decode sol x) (decode sol y)
  decode sol (ArrSelect i arr)  = liftA2 select (decode sol i) (decode sol arr)
  decode sol (ArrStore i x arr) = liftM3 store (decode sol i) (decode sol x) (decode sol arr)
  decode _ (ForAll _ _)         = Nothing
  decode _ (Exists _ _)         = Nothing
  
  encode = Constant . wrapValue

instance Codec () where
  type Decoded () = ()
  decode _ _ = Just ()
  encode _   = ()

instance (Codec a, Codec b) => Codec (a,b) where
  type Decoded (a,b) = (Decoded a, Decoded b)
  decode s (a,b) = (,) <$> decode s a <*> decode s b
  encode   (a,b) = (encode a, encode b)

instance (Codec a, Codec b, Codec c) => Codec (a,b,c) where
  type Decoded (a,b,c) = (Decoded a, Decoded b, Decoded c)
  decode s (a,b,c) = (,,) <$> decode s a <*> decode s b <*> decode s c
  encode   (a,b,c) = (encode a, encode b, encode c)

instance (Codec a, Codec b, Codec c, Codec d) => Codec (a,b,c,d) where
  type Decoded (a,b,c,d) = (Decoded a, Decoded b, Decoded c, Decoded d)
  decode s (a,b,c,d) = (,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d
  encode   (a,b,c,d) = (encode a, encode b, encode c, encode d)

instance (Codec a, Codec b, Codec c, Codec d, Codec e) => Codec (a,b,c,d,e) where
  type Decoded (a,b,c,d,e) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e)
  decode s (a,b,c,d,e) = (,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e
  encode   (a,b,c,d,e) = (encode a, encode b, encode c, encode d, encode e)

instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f) => Codec (a,b,c,d,e,f) where
  type Decoded (a,b,c,d,e,f) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f)
  decode s (a,b,c,d,e,f) = (,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f
  encode   (a,b,c,d,e,f) = (encode a, encode b, encode c, encode d, encode e, encode f)

instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f, Codec g) => Codec (a,b,c,d,e,f,g) where
  type Decoded (a,b,c,d,e,f,g) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g)
  decode s (a,b,c,d,e,f,g) = (,,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g
  encode   (a,b,c,d,e,f,g) = (encode a, encode b, encode c, encode d, encode e, encode f, encode g)

instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f, Codec g, Codec h) => Codec (a,b,c,d,e,f,g,h) where
  type Decoded (a,b,c,d,e,f,g,h) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g, Decoded h)
  decode s (a,b,c,d,e,f,g,h) = (,,,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g <*> decode s h
  encode   (a,b,c,d,e,f,g,h) = (encode a, encode b, encode c, encode d, encode e, encode f, encode g, encode h)

instance Codec a => Codec [a]
instance Codec a => Codec (IntMap a)
instance Codec a => Codec (Map k a)
instance Codec a => Codec (Maybe a)
instance Codec a => Codec (Seq a)
instance Codec a => Codec (Tree a)

instance (Codec a, Codec b) => Codec (Either a b) where
  type Decoded (Either a b) = Either (Decoded a) (Decoded b)
  decode s (Left  a) = Left  <$> decode s a
  decode s (Right b) = Right <$> decode s b
  encode   (Left  a) = Left  (encode a)
  encode   (Right b) = Right (encode b)
