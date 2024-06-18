module Language.Hasmtlib.Internal.Expr.Num where

import Prelude hiding (div, mod, quotRem, rem, quot, divMod)
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Integraled
import Language.Hasmtlib.Iteable
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Orderable  
import Data.Proxy
import GHC.TypeNats
  
instance Num (Expr IntSort) where
   fromInteger = Constant . IntValue
   {-# INLINE fromInteger #-}
   (+)         = Plus
   {-# INLINE (+) #-}
   (-) x y     = Plus x (Neg y)
   {-# INLINE (-) #-}
   (*)         = Mul
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
   (+)         = Plus
   {-# INLINE (+) #-}
   x - y       = Plus x (Neg y)
   {-# INLINE (-) #-}
   (*)         = Mul
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
   (+)         = BvAdd
   {-# INLINE (+) #-}
   (-)         = BvSub
   {-# INLINE (-) #-}
   (*)         = BvMul
   {-# INLINE (*) #-}
   abs         = id
   {-# INLINE abs #-}
   signum _    = 0
   {-# INLINE signum #-}
   
instance Fractional (Expr RealSort) where
  fromRational = Constant . RealValue . fromRational
  {-# INLINE fromRational #-}
  (/)          = Div
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

-- | Bitvector shift left
bvShL    :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
bvShL    = BvShL
{-# INLINE bvShL #-}

-- | Bitvector logical shift right
bvLShR   :: KnownNat n => Expr (BvSort n) -> Expr (BvSort n) -> Expr (BvSort n)
bvLShR   = BvLShR
{-# INLINE bvLShR #-}

-- | Concat two bitvectors
bvConcat :: (KnownNat n, KnownNat m) => Expr (BvSort n) -> Expr (BvSort m) -> Expr (BvSort (n + m))
bvConcat = BvConcat
{-# INLINE bvConcat #-}

-- | Rotate bitvector left
bvRotL   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
bvRotL   = BvRotL
{-# INLINE bvRotL #-}

-- | Rotate bitvector right
bvRotR   :: (KnownNat n, KnownNat i, KnownNat (Mod i n)) => Proxy i -> Expr (BvSort n) -> Expr (BvSort n)
bvRotR   = BvRotR
{-# INLINE bvRotR #-}
