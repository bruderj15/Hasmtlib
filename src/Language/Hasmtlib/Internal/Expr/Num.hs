-- Required for class constraints of form: c (ValueType t) :: Constraint
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Internal.Expr.Num where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Iteable
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Orderable  
import Data.Proxy
import GHC.TypeNats
  
instance Num (Expr IntType) where
   fromInteger = Constant . IntValue
   (+)     = Plus
   (-) x y = Plus x (Neg y)
   (*)     = Mul
   negate  = Neg
   abs     = Abs
   signum x   = ite ((x === 0) :: Expr BoolType)
                  0 $
                  ite ((x <? 0) :: Expr BoolType) (-1) 1

instance Num (Expr RealType) where
   fromInteger = Constant . RealValue . fromIntegral
   (+)      = Plus
   x - y    = Plus x (Neg y)
   (*)      = Mul
   negate   = Neg
   abs      = Abs
   signum x = ite ((x === 0) :: Expr BoolType)
                  0 $
                  ite ((x <? 0) :: Expr BoolType) (-1) 1

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
    log   = Log
    sqrt  = Sqrt
    sin   = Sin
    cos   = Cos
    tan   = Tan
    asin  = Asin
    acos  = Acos
    atan  = Atan
    sinh  = Sinh
    cosh  = Cosh
    tanh  = Tanh
    asinh = Asinh
    acosh = Acosh
    atanh = Atanh

mod' :: Expr IntType -> Expr IntType -> Expr IntType
mod' = Mod

div' :: Expr IntType -> Expr IntType -> Expr IntType
div' = IDiv

bvuDiv   :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
bvuDiv   = BvuDiv

bvuRem   :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
bvuRem   = BvuRem

bvShL    :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
bvShL    = BvShL

bvLShR   :: KnownNat n => Expr (BvType n) -> Expr (BvType n) -> Expr (BvType n)
bvLShR   = BvLShR

bvConcat :: (KnownNat n, KnownNat m) => Expr (BvType n) -> Expr (BvType m) -> Expr (BvType (n + m))
bvConcat = BvConcat

bvRotL   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvType n) -> Expr (BvType n)
bvRotL   = BvRotL

bvRotR   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvType n) -> Expr (BvType n)
bvRotR   = BvRotR