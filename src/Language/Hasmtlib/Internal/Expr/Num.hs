module Language.Hasmtlib.Internal.Expr.Num where

import Prelude hiding (div, mod, quotRem, rem, quot, divMod)
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Integraled
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

instance Integraled (Expr IntType) where
  quot = IDiv
  rem  = Mod
  div  = IDiv
  mod  = Mod
  quotRem x y = (quot x y, rem x y)
  divMod x y  = (div x y, mod x y)

instance KnownNat n => Integraled (Expr (BvType n)) where
  quot        = BvuDiv
  rem         = BvuRem
  div         = BvuDiv
  mod         = BvuRem
  quotRem x y = (quot x y, rem x y)
  divMod x y  = (div x y, mod x y)

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