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
    
class    SMTNumber (t :: SMTType) where smtValueFromInteger :: Integer -> Value t
instance SMTNumber RealType       where smtValueFromInteger = RealValue . fromIntegral
instance SMTNumber IntType        where smtValueFromInteger = IntValue  
  
instance (KnownSMTRepr t, SMTNumber t, Num (ValueType t), Ord (ValueType t)) => Num (Expr t) where
  fromInteger = Constant . smtValueFromInteger
  (+)         = Plus
  x - y       = Plus x (Neg y)
  (*)         = Mul
  negate      = Neg
  abs         = Abs
  signum x    = ite (x === 0) 0 $ ite (x <? 0) (-1) 1

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

