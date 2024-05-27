module Language.Hasmtlib.Codec where

import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Boolean
import Data.Kind
import Data.Coerce
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.IntMap as IM
import Data.Tree (Tree)
import Control.Monad

class Codec a where
  type Decoded a :: Type
  decode :: Solution -> a -> Maybe (Decoded a)

instance KnownSMTRepr t => Codec (SMTVar t) where
  type Decoded (SMTVar t) = ValueType t
  decode solution var = do
    someSol <- IM.lookup (coerce var) solution
    case singRepr @t of
      IntRepr   -> case someSol of
                    SomeKnownSMTRepr (SMTVarSol _ (IntValue v))  -> Just v
                    _                                            -> Nothing
      RealRepr  -> case someSol of
                    SomeKnownSMTRepr (SMTVarSol _ (RealValue v)) -> Just v
                    _                                            -> Nothing
      BoolRepr  -> case someSol of
                    SomeKnownSMTRepr (SMTVarSol _ (BoolValue v)) -> Just v
                    _                                            -> Nothing

instance KnownSMTRepr t => Codec (Expr t) where
  type Decoded (Expr t) = ValueType t
  decode sol (Var v)    = decode sol v
  decode _ (Constant v) = Just $ extractValue v

  decode sol (Plus x y) = liftA2 (+)   (decode sol x) (decode sol y)
  decode sol (Neg x)    = fmap negate  (decode sol x)
  decode sol (Mul x y)  = liftA2 (-)   (decode sol x) (decode sol y)
  decode sol (Abs x)    = fmap abs     (decode sol x)
  decode sol (Mod x y)  = liftA2 mod   (decode sol x) (decode sol y)
  decode sol (Div x y)  = liftA2 (/)   (decode sol x) (decode sol y)

  decode sol (LTH x y)  = liftA2 (<)   (decode sol x) (decode sol y)
  decode sol (LTHE x y) = liftA2 (<=)  (decode sol x) (decode sol y)
  decode sol (EQU x y)  = liftA2 (==)  (decode sol x) (decode sol y)
  decode sol (GTHE x y) = liftA2 (>=)  (decode sol x) (decode sol y)
  decode sol (GTH x y)  = liftA2 (>)   (decode sol x) (decode sol y)
  
  decode sol (Not x)    = fmap   not'  (decode sol x)
  decode sol (And x y)  = liftA2 (&&&) (decode sol x) (decode sol y)
  decode sol (Or x y)   = liftA2 (|||) (decode sol x) (decode sol y)
  decode sol (Impl x y) = liftA2 (==>) (decode sol x) (decode sol y)
  decode sol (Xor x y)  = liftA2 xor   (decode sol x) (decode sol y)

  decode _ Pi           = Just pi
  decode sol (Sqrt x)   = fmap sqrt  (decode sol x)
  decode sol (Exp x)    = fmap exp   (decode sol x)
  decode sol (Log x)    = fmap log   (decode sol x)
  decode sol (Sin x)    = fmap sin   (decode sol x)
  decode sol (Cos x)    = fmap cos   (decode sol x)
  decode sol (Tan x)    = fmap tan   (decode sol x)
  decode sol (Asin x)   = fmap asin  (decode sol x)
  decode sol (Acos x)   = fmap acos  (decode sol x)
  decode sol (Atan x)   = fmap atan  (decode sol x)
  decode sol (Sinh x)   = fmap sinh  (decode sol x)
  decode sol (Cosh x)   = fmap cosh  (decode sol x)
  decode sol (Tanh x)   = fmap tanh  (decode sol x)
  decode sol (Asinh x)  = fmap asinh (decode sol x)
  decode sol (Acosh x)  = fmap acosh (decode sol x)
  decode sol (Atanh x)  = fmap atanh (decode sol x)

  decode sol (ToReal x) = fmap realToFrac (decode sol x)
  decode sol (ToInt x)  = fmap truncate   (decode sol x)
  decode sol (IsInt x)  = fmap ((0 ==) . snd . properFraction) (decode sol x)

  decode sol (Ite p t f) = liftM3 (\p' t' f' -> if p' then t' else f') (decode sol p) (decode sol t) (decode sol f) 

instance Codec () where
  type Decoded () = ()
  decode _ _ = Just ()

instance (Codec a, Codec b) => Codec (a,b) where
  type Decoded (a,b) = (Decoded a, Decoded b)
  decode s (a,b) = (,) <$> decode s a <*> decode s b

instance (Codec a, Codec b, Codec c) => Codec (a,b,c) where
  type Decoded (a,b,c) = (Decoded a, Decoded b, Decoded c)
  decode s (a,b,c) = (,,) <$> decode s a <*> decode s b <*> decode s c

instance (Codec a, Codec b, Codec c, Codec d) => Codec (a,b,c,d) where
  type Decoded (a,b,c,d) = (Decoded a, Decoded b, Decoded c, Decoded d)
  decode s (a,b,c,d) = (,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d

instance (Codec a, Codec b, Codec c, Codec d, Codec e) => Codec (a,b,c,d,e) where
  type Decoded (a,b,c,d,e) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e)
  decode s (a,b,c,d,e) = (,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e

instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f) => Codec (a,b,c,d,e,f) where
  type Decoded (a,b,c,d,e,f) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f)
  decode s (a,b,c,d,e,f) = (,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f

instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f, Codec g) => Codec (a,b,c,d,e,f,g) where
  type Decoded (a,b,c,d,e,f,g) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g)
  decode s (a,b,c,d,e,f,g) = (,,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g

instance (Codec a, Codec b, Codec c, Codec d, Codec e, Codec f, Codec g, Codec h) => Codec (a,b,c,d,e,f,g,h) where
  type Decoded (a,b,c,d,e,f,g,h) = (Decoded a, Decoded b, Decoded c, Decoded d, Decoded e, Decoded f, Decoded g, Decoded h)
  decode s (a,b,c,d,e,f,g,h) = (,,,,,,,) <$> decode s a <*> decode s b <*> decode s c <*> decode s d <*> decode s e <*> decode s f <*> decode s g <*> decode s h

instance Codec a => Codec [a] where
  type Decoded [a] = [Decoded a]
  decode = mapM . decode

instance (Codec a, Codec b) => Codec (Either a b) where
  type Decoded (Either a b) = Either (Decoded a) (Decoded b)
  decode s (Left  a) = Left  <$> decode s a
  decode s (Right b) = Right <$> decode s b

instance Codec a => Codec (IntMap a) where
  type Decoded (IntMap a) = IntMap (Decoded a)
  decode = mapM . decode

instance Codec a => Codec (Map k a) where
  type Decoded (Map k a) = Map k (Decoded a)
  decode = mapM . decode

instance Codec a => Codec (Maybe a) where
  type Decoded (Maybe a) = Maybe (Decoded a)
  decode = mapM . decode

instance Codec a => Codec (Seq a) where
  type Decoded (Seq a) = Seq (Decoded a)
  decode = mapM . decode

instance Codec a => Codec (Tree a) where
  type Decoded (Tree a) = Tree (Decoded a)
  decode = mapM . decode