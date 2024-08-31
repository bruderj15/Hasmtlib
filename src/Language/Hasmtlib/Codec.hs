{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Hasmtlib.Codec where

import Prelude hiding (not, (&&), (||), all, and)
import Language.Hasmtlib.Type.Bitvec
import Language.Hasmtlib.Type.Expr (Expr(..), SMTVar(..))
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Type.ArrayMap
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.Value
import Language.Hasmtlib.Boolean
import Data.Kind
import Data.Proxy
import Data.Coerce
import qualified Data.List as List
import Data.Bits hiding (And, Xor, xor)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.IntMap as IM hiding (foldl)
import Data.Dependent.Map as DMap
import Data.Tree (Tree)
import Data.Array (Array, Ix)
import qualified Data.Text as Text
import Data.Monoid (Sum, Product, First, Last, Dual)
import qualified Data.Vector.Sized as V
import Control.Monad
import Control.Lens hiding (from, to)
import GHC.Generics
import GHC.TypeLits

-- | Computes a default 'Decoded' 'Type' by distributing 'Decoded' to it's type arguments.
type family DefaultDecoded a :: Type where
  DefaultDecoded (t a b c d e f g h) = t (Decoded a) (Decoded b) (Decoded c) (Decoded d) (Decoded e) (Decoded f) (Decoded g) (Decoded h)
  DefaultDecoded (t a b c d e f g) = t (Decoded a) (Decoded b) (Decoded c) (Decoded d) (Decoded e) (Decoded f) (Decoded g)
  DefaultDecoded (t a b c d e f) = t (Decoded a) (Decoded b) (Decoded c) (Decoded d) (Decoded e) (Decoded f)
  DefaultDecoded (t a b c d e) = t (Decoded a) (Decoded b) (Decoded c) (Decoded d) (Decoded e)
  DefaultDecoded (t a b c d) = t (Decoded a) (Decoded b) (Decoded c) (Decoded d)
  DefaultDecoded (t a b c) = t (Decoded a) (Decoded b) (Decoded c)
  DefaultDecoded (t a b) = t (Decoded a) (Decoded b)
  DefaultDecoded (t a) = t (Decoded a)
  DefaultDecoded x = TypeError (
          Text "DefaultDecoded (" :<>: ShowType x :<>: Text ") is not allowed."
    :$$:  Text "Try providing the associated Type Decoded (" :<>: ShowType x :<>: Text ") manually:"
    :$$:  Text "instance Codec (" :<>: ShowType x :<>: Text ") where "
    :$$:  Text "   type Decoded (" :<>: ShowType x :<>: Text ") = ... "
    )

-- | Lift values to SMT-Values or decode them.
--
--   You can derive an instance of this class if your type is 'Generic'.
class Codec a where
  -- | Result of decoding @a@.
  type Decoded a :: Type
  type Decoded a = DefaultDecoded a

  -- | Decode a value using given solution.
  decode :: Solution -> a -> Maybe (Decoded a)
  default decode :: (Generic a, Generic (Decoded a), GCodec (Rep a), GDecoded (Rep a) x ~ Rep (Decoded a) x) => Solution -> a -> Maybe (Decoded a)
  decode sol x = do
    gdecodedx <- gdecode sol $ from x
    Just $ to gdecodedx

  -- | Encode a value as constant.
  encode :: Decoded a -> a
  default encode :: (Generic a, Generic (Decoded a), GCodec (Rep a), GDecoded (Rep a) x ~ Rep (Decoded a) x) => Decoded a -> a
  encode = to . gencode . from

-- | Decode and evaluate expressions
instance KnownSMTSort t => Codec (Expr t) where
  type Decoded (Expr t) = HaskellType t
  decode sol (Var var)  = do
    let sungSort = sortSing @t
    (IntValueMap m) <- case sungSort of
      SBvSort enc n -> case bvEncSing' enc of
      -- Solution contains all BV as unsigned, if we have a Signed one we check the Unsigned ones and flip BvEnc
        SUnsigned -> DMap.lookup sungSort sol
        SSigned -> DMap.lookup (SBvSort (Proxy @Unsigned) n) sol <&>
          \case (IntValueMap ubvs) -> IntValueMap $ fmap (\case (BvValue ubv) -> BvValue $ asSigned ubv) ubvs
      _ -> DMap.lookup sungSort sol
    val <- IM.lookup (coerce var) m
    return $ unwrapValue val
  decode _ (Constant v)         = Just $ unwrapValue v
  decode sol (Plus x y)         = (+)   <$> decode sol x <*> decode sol y
  decode sol (Minus x y)        = (-)   <$> decode sol x <*> decode sol y
  decode sol (Neg x)            = fmap negate  (decode sol x)
  decode sol (Mul x y)          = (*)   <$> decode sol x <*> decode sol y
  decode sol (Abs x)            = fmap abs     (decode sol x)
  decode sol (Mod x y)          = mod   <$> decode sol x <*> decode sol y
  decode sol (Rem x y)          = rem   <$> decode sol x <*> decode sol y
  decode sol (IDiv x y)         = div   <$> decode sol x <*> decode sol y
  decode sol (Div x y)          = (/)   <$> decode sol x <*> decode sol y
  decode sol (LTH x y)          = (<)   <$> decode sol x <*> decode sol y
  decode sol (LTHE x y)         = (<=)  <$> decode sol x <*> decode sol y
  decode sol (EQU xs)           = do
    xs' <- decode sol (V.toList xs)
    case xs' of
      []   -> return true
      (x:xs'') -> return $ all (x ==) xs''
  decode sol (Distinct xs)      = do
    xs' <- decode sol (V.toList xs)
    let xss = List.filter ((==2) . length) $ List.permutations xs'
    return $ all (\case (a:b:_) -> a /= b ; _ -> true) xss
  decode sol (GTHE x y)         = (>=)  <$> decode sol x <*> decode sol y
  decode sol (GTH x y)          = (>)   <$> decode sol x <*> decode sol y
  decode sol (Not x)            = fmap   not  (decode sol x)
  decode sol (And x y)          = (&&) <$> decode sol x <*> decode sol y
  decode sol (Or x y)           = (||) <$> decode sol x <*> decode sol y
  decode sol (Impl x y)         = (==>) <$> decode sol x <*> decode sol y
  decode sol (Xor x y)          = xor   <$> decode sol x <*> decode sol y
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
  decode sol (Ite p t f)        = (\p' t' f' -> if p' then t' else f') <$> decode sol p <*> decode sol t <*> decode sol f
  decode sol (BvNand x y)       = nand <$> sequenceA [decode sol x, decode sol y]
  decode sol (BvNor x y)        = nor  <$> sequenceA [decode sol x, decode sol y]
  decode sol (BvShL x y)        = do
    x' <- decode sol x
    y' <- decode sol y
    return $ shiftL x' $ fromIntegral (toInteger y')
  decode sol (BvLShR x y)       = do
    x' <- decode sol x
    y' <- decode sol y
    return $ shiftR x' $ fromIntegral (toInteger y')
  decode sol (BvAShR x y)       = do
    x' <- decode sol x
    y' <- decode sol y
    return $ shiftR x' $ fromIntegral (toInteger y')
  decode sol (BvConcat x y)     = bitvecConcat <$> decode sol x <*> decode sol y
  decode sol (BvRotL i x)       = rotateL <$> decode sol x <*> pure (fromIntegral i)
  decode sol (BvRotR i x)       = rotateR <$> decode sol x <*> pure (fromIntegral i)
  decode sol (ArrSelect i arr)  = arrSelect <$> decode sol i <*> decode sol arr
  decode sol (ArrStore i x arr) = arrStore <$> decode sol i <*> decode sol x <*> decode sol arr
  decode sol (StrConcat x y)         = (<>) <$> decode sol x <*> decode sol y
  decode sol (StrLength x)           = toInteger . Text.length <$> decode sol x
  decode sol (StrAt x i)             = (\x' i' -> Text.singleton $ Text.index x' (fromInteger i')) <$> decode sol x <*> decode sol i
  decode sol (StrSubstring x i j)    = (\x' (fromInteger -> i') (fromInteger -> j') -> Text.take (j' - i') $ Text.drop i' x') <$> decode sol x <*> decode sol i <*> decode sol j
  decode sol (StrPrefixOf x y)       = Text.isPrefixOf <$> decode sol x <*> decode sol y
  decode sol (StrSuffixOf x y)       = Text.isSuffixOf <$> decode sol x <*> decode sol y
  decode sol (StrContains x y)       = flip Text.isInfixOf <$> decode sol x <*> decode sol y
  decode sol (StrIndexOf x y i)      = join $ (\x' y' (fromInteger -> i') -> Text.findIndex ((y' ==) . Text.singleton) (Text.drop i' x') >>= Just . toInteger) <$> decode sol x <*> decode sol y <*> decode sol i
  decode sol (StrReplace src target replacement) = (\src' target' replacement' -> replaceOne target' replacement' src') <$> decode sol target <*> decode sol src <*> decode sol replacement
    where
      replaceOne pattern substitution text
        | Text.null back = text
        | otherwise = Text.concat [front, substitution, Text.drop (Text.length pattern) back]
          where
            (front, back) = Text.breakOn pattern text
  decode sol (StrReplaceAll src target replacement) = (\src' target' replacement' -> Text.replace target' replacement' src') <$> decode sol target <*> decode sol src <*> decode sol replacement
  decode _ (ForAll _ _)         = Nothing
  decode _ (Exists _ _)         = Nothing
  encode = Constant . wrapValue

instance Codec () where type Decoded () = ()
instance (Codec a, Codec b) => Codec (a,b)
instance (Codec a, Codec b, Codec c) => Codec (a,b,c)
instance (Codec a, Codec b, Codec c, Codec d) => Codec (a,b,c,d)
instance (Codec a, Codec b, Codec c, Codec d, Codec e) => Codec (a,b,c,d,e)
instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f) => Codec (a,b,c,d,e,f)
instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f, Codec g) => Codec (a,b,c,d,e,f,g)
instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f, Codec g, Codec h) => Codec (a,b,c,d,e,f,g,h)
instance Codec a => Codec [a]
instance Codec a => Codec (Maybe a)
instance Codec a => Codec (Tree a)
instance (Codec a, Codec b) => Codec (Either a b)
instance Codec a => Codec (Sum a)
instance Codec a => Codec (Product a)
instance Codec a => Codec (First a)
instance Codec a => Codec (Last a)
instance Codec a => Codec (Dual a)
instance Codec a => Codec (Identity a)

instance Codec a => Codec (IntMap a) where
  decode = traverse . decode
  encode = fmap encode

instance Codec a => Codec (Seq a) where
  decode = traverse . decode
  encode = fmap encode

instance Codec a => Codec (Map k a) where
  type Decoded (Map k a) = Map k (Decoded a)
  decode = traverse . decode
  encode = fmap encode

instance (Ix i, Codec e) => Codec (Array i e) where
  type Decoded (Array i e) = Array i (Decoded e)
  decode = traverse . decode
  encode = fmap encode

class GCodec f where
  type GDecoded f :: Type -> Type
  gdecode :: Solution -> f a -> Maybe (GDecoded f a)
  gencode :: GDecoded f a -> f a

instance GCodec U1 where
  type GDecoded U1 = U1
  gdecode _ U1     = Just U1
  gencode          = id

instance GCodec V1 where
  type GDecoded V1 = V1
  gdecode _        = Just
  gencode          = id

instance (GCodec f, GCodec g) => GCodec (f :*: g) where
  type GDecoded (f :*: g) = (GDecoded f :*: GDecoded g)
  gdecode sol (a :*: b)   = (:*:) <$> gdecode sol a <*> gdecode sol b
  gencode (a :*: b)       = gencode a :*: gencode b

instance (GCodec f, GCodec g) => GCodec (f :+: g) where
  type GDecoded (f :+: g) = (GDecoded f :+: GDecoded g)
  gdecode sol (L1 a)      = L1 <$> gdecode sol a
  gdecode sol (R1 a)      = R1 <$> gdecode sol a
  gencode (L1 a)          = L1 $ gencode a
  gencode (R1 a)          = R1 $ gencode a

instance GCodec f => GCodec (M1 i c f) where
  type GDecoded (M1 i c f) = (M1 i c (GDecoded f))
  gdecode sol (M1 x)       = M1 <$> gdecode sol x
  gencode (M1 x)           = M1 $ gencode x

instance Codec a => GCodec (K1 i a) where
  type GDecoded (K1 i a) = K1 i (Decoded a)
  gdecode sol (K1 a)     = K1 <$> decode sol a
  gencode (K1 a)         = K1 $ encode a
