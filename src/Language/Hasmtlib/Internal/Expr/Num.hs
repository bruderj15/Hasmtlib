module Language.Hasmtlib.Internal.Expr.Num where

import Prelude hiding (div, mod, quotRem, rem, quot, divMod)
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Integraled
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Iteable
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Orderable
import GHC.TypeNats

instance Num (Expr IntSort) where
   fromInteger = Constant . IntValue
   {-# INLINE fromInteger #-}
   (Constant (IntValue 0)) + y = y
   x + (Constant (IntValue 0)) = x
   (Constant (IntValue x)) + (Constant (IntValue y)) = Constant (IntValue (x + y))
   x + y = Plus x y
   {-# INLINE (+) #-}
   x - (Constant (IntValue 0)) = x
   (Constant (IntValue x)) - (Constant (IntValue y)) = Constant (IntValue (x - y))
   x - y = Plus x (Neg y)
   {-# INLINE (-) #-}
   (Constant (IntValue 0)) * _ = 0
   _ * (Constant (IntValue 0)) = 0
   (Constant (IntValue 1)) * y = y
   x * (Constant (IntValue 1)) = x
   (Constant (IntValue x)) * (Constant (IntValue y)) = Constant (IntValue (x * y))
   x * y = Mul x y
   {-# INLINE (*) #-}
   negate      = Neg
   {-# INLINE negate #-}
   abs         = Abs
   {-# INLINE abs #-}
   signum x    = ite (x === 0) 0 $ ite (x <? 0) (-1) 1
   {-# INLINE signum #-}

instance Num (Expr RealSort) where
   fromInteger = Constant . RealValue . fromIntegral
   {-# INLINE fromInteger #-}
   (Constant (RealValue 0)) + y = y
   x + (Constant (RealValue 0)) = x
   (Constant (RealValue x)) + (Constant (RealValue y)) = Constant (RealValue (x + y))
   x + y = Plus x y
   {-# INLINE (+) #-}
   x - (Constant (RealValue 0)) = x
   (Constant (RealValue x)) - (Constant (RealValue y)) = Constant (RealValue (x - y))
   x - y = Plus x (Neg y)
   {-# INLINE (-) #-}
   (Constant (RealValue 0)) * _ = 0
   _ * (Constant (RealValue 0)) = 0
   (Constant (RealValue 1)) * y = y
   x * (Constant (RealValue 1)) = x
   (Constant (RealValue x)) * (Constant (RealValue y)) = Constant (RealValue (x * y))
   x * y = Mul x y
   {-# INLINE (*) #-}
   negate      = Neg
   {-# INLINE negate #-}
   abs         = Abs
   {-# INLINE abs #-}
   signum x    = ite (x === 0) 0 $ ite (x <? 0) (-1) 1
   {-# INLINE signum #-}

instance KnownNat n => Num (Expr (BvSort n)) where
   fromInteger = Constant . BvValue . fromInteger
   {-# INLINE fromInteger #-}
   (Constant (BvValue 0)) + y = y
   x + (Constant (BvValue 0)) = x
   (Constant (BvValue x)) + (Constant (BvValue y)) = Constant (BvValue (x + y))
   x + y = BvAdd x y
   {-# INLINE (+) #-}
   x - (Constant (BvValue 0)) = x
   (Constant (BvValue x)) - (Constant (BvValue y)) = Constant (BvValue (x - y))
   x - y = BvSub x y
   {-# INLINE (-) #-}
   (Constant (BvValue 0)) * _ = 0
   _ * (Constant (BvValue 0)) = 0
   (Constant (BvValue 1)) * y = y
   x * (Constant (BvValue 1)) = x
   (Constant (BvValue x)) * (Constant (BvValue y)) = Constant (BvValue (x * y))
   x * y = BvMul x y
   {-# INLINE (*) #-}
   abs         = id
   {-# INLINE abs #-}
   signum _    = 0
   {-# INLINE signum #-}

instance Fractional (Expr RealSort) where
  fromRational = Constant . RealValue . fromRational
  {-# INLINE fromRational #-}
  x / (Constant (RealValue 1)) = x
  (Constant (RealValue 0)) / _ = 0
  (Constant (RealValue x)) / (Constant (RealValue y)) = Constant (RealValue (x / y))
  x / y          = Div x y
  {-# INLINE (/) #-}

instance Floating (Expr RealSort) where
    pi    = Pi
    {-# INLINE pi #-}
    exp   = Exp
    {-# INLINE exp #-}
    log   = error "SMT-Solvers currently do not support log"
    sqrt  = Sqrt
    {-# INLINE sqrt #-}
    sin   = Sin
    {-# INLINE sin #-}
    cos   = Cos
    {-# INLINE cos #-}
    tan   = Tan
    {-# INLINE tan #-}
    asin  = Asin
    {-# INLINE asin #-}
    acos  = Acos
    {-# INLINE acos #-}
    atan  = Atan
    {-# INLINE atan #-}
    sinh  = error "SMT-Solver currently do not support sinh"
    cosh  = error "SMT-Solver currently do not support cosh"
    tanh  = error "SMT-Solver currently do not support tanh"
    asinh = error "SMT-Solver currently do not support asinh"
    acosh = error "SMT-Solver currently do not support acosh"
    atanh = error "SMT-Solver currently do not support atanh"

instance Integraled (Expr IntSort) where
  quot = IDiv
  {-# INLINE quot #-}
  rem  = Mod
  {-# INLINE rem #-}
  div  = IDiv
  {-# INLINE div #-}
  mod  = Mod
  {-# INLINE mod #-}
  quotRem x y = (quot x y, rem x y)
  {-# INLINE quotRem #-}
  divMod x y  = (div x y, mod x y)
  {-# INLINE divMod #-}

instance KnownNat n => Integraled (Expr (BvSort n)) where
  quot        = BvuDiv
  {-# INLINE quot #-}
  rem         = BvuRem
  {-# INLINE rem #-}
  div         = BvuDiv
  {-# INLINE div #-}
  mod         = BvuRem
  {-# INLINE mod #-}
  quotRem x y = (quot x y, rem x y)
  {-# INLINE quotRem #-}
  divMod x y  = (div x y, mod x y)
  {-# INLINE divMod #-}
