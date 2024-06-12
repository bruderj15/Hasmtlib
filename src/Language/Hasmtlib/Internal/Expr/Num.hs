module Language.Hasmtlib.Internal.Expr.Num where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Iteable
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Orderable  
import Data.Proxy
import GHC.TypeNats
  
instance Num (Expr IntType) where
   fromInteger = Constant . IntValue
   (+)         = Plus
   (-) x y     = Plus x (Neg y)
   (*)         = Mul
   negate      = Neg
   abs         = Abs
   signum x    = ite (x === 0) 0 $ ite (x <? 0) (-1) 1

instance Num (Expr RealType) where
   fromInteger = Constant . RealValue . fromIntegral
   (+)         = Plus
   x - y       = Plus x (Neg y)
   (*)         = Mul
   negate      = Neg
   abs         = Abs
   signum x    = ite (x === 0) 0 $ ite (x <? 0) (-1) 1

instance KnownNat n => Num (Expr (BvType n)) where
   fromInteger = Constant . BvValue . fromInteger 
   (+)         = BvAdd
   (-)         = BvSub
   (*)         = BvMul
   abs         = id
   signum _    = 0

instance Fractional (Expr RealType) where
  fromRational = Constant . RealValue . fromRational
  (/)          = Div

instance Floating (Expr RealType) where
    pi    = Pi
    exp   = Exp
    log   = error "SMT-Solvers currently do not support log"
    sqrt  = Sqrt
    sin   = Sin
    cos   = Cos
    tan   = Tan
    asin  = Asin
    acos  = Acos
    atan  = Atan
    sinh  = error "SMT-Solver currently do not support sinh"
    cosh  = error "SMT-Solver currently do not support cosh"
    tanh  = error "SMT-Solver currently do not support tanh"
    asinh = error "SMT-Solver currently do not support asinh"
    acosh = error "SMT-Solver currently do not support acosh"
    atanh = error "SMT-Solver currently do not support atanh"

-- | Integer modulus
mod' :: Expr IntType -> Expr IntType -> Expr IntType
mod' = Mod

-- | Integer division
div' :: Expr IntType -> Expr IntType -> Expr IntType
div' = IDiv

-- | Unsigned bitvector remainder
bvuRem   :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
bvuRem   = BvuRem

-- | Bitvector shift left
bvShL    :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
bvShL    = BvShL

-- | Bitvector logical shift right
bvLShR   :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
bvLShR   = BvLShR

-- | Concat two bitvectors
bvConcat :: (KnownNat n, KnownNat m) => Expr (BvType n) -> Expr (BvType m) -> Expr (BvType (n + m))
bvConcat = BvConcat

-- | Rotate bitvector left
bvRotL   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvType n) -> Expr (BvType n)
bvRotL   = BvRotL

-- | Rotate bitvector right
bvRotR   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvType n) -> Expr (BvType n)
bvRotR   = BvRotR