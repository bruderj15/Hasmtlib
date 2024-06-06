-- Required for class constraints of form: c (ValueType t) :: Constraint
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Internal.Expr.Num 
(
  Num(..)
, Fractional(..)
, Floating(..)
) where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Iteable
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Orderable  
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

