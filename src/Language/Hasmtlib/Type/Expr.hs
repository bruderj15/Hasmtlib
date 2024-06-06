module Language.Hasmtlib.Type.Expr
 ( SMTType(..)
 , SMTVar(..)
 , ValueType
 , Value(..), extractValue, putValue
 , Repr(..), KnownSMTRepr(..), SomeKnownSMTRepr(..)
 , Expr
 , module Language.Hasmtlib.Internal.Expr.Num
 , bvuDiv, bvuRem, bvShL, bvLShR, bvConcat, bvRotL, bvRotR 
 )
where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Expr.Num
import Data.Proxy
import GHC.TypeNats

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
