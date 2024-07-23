{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Hasmtlib.Type.Expr
 ( SMTVar(..), varId
 , Value(..), unwrapValue, wrapValue
 , Expr
 , equal, distinct
 , bvShL, bvLShR, bvConcat, bvRotL, bvRotR
 , toInt, toReal, isInt
 , for_all , exists
 , select, store
 )
where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Expr.Num ()
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Boolean
import Data.Proxy
import Data.List (genericLength)
import Data.Foldable (toList)
import qualified Data.Vector.Sized as V
import GHC.TypeNats

-- | Test multiple expressions on equality within in the 'SMT'-Problem.
equal :: (Eq (HaskellType t), KnownSMTSort t, Foldable f) => f (Expr t) -> Expr BoolSort
equal (toList -> (a:b:xs)) = case someNatVal (genericLength xs) of
  SomeNat n -> case V.fromListN' n xs of
    Nothing  -> EQU $ V.fromTuple (a,b)
    Just xs' -> EQU $ xs' V.++ V.fromTuple (a,b)
equal (toList -> _)        = true

-- | Test multiple expressions on distinctness within in the 'SMT'-Problem.
distinct :: (Eq (HaskellType t), KnownSMTSort t, Foldable f) => f (Expr t) -> Expr BoolSort
distinct (toList -> (a:b:xs)) = case someNatVal (genericLength xs) of
  SomeNat n -> case V.fromListN' n xs of
    Nothing  -> Distinct $ V.fromTuple (a,b)
    Just xs' -> Distinct $ xs' V.++ V.fromTuple (a,b)
distinct (toList -> _)        = true

-- | A universal quantification for any specific 'SMTSort'.
--   If the type cannot be inferred, apply a type-annotation.
--   Nested quantifiers are also supported.
--
--   Usage:
--
--   @
--   assert $
--      for_all @IntSort $ \x ->
--         x + 0 === x && 0 + x === x
--   @
--
--   The lambdas 'x' is all-quantified here.
--   It will only be scoped for the lambdas body.
for_all :: forall t. KnownSMTSort t => (Expr t -> Expr BoolSort) -> Expr BoolSort
for_all = ForAll Nothing

-- | An existential quantification for any specific 'SMTSort'
--   If the type cannot be inferred, apply a type-annotation.
--   Nested quantifiers are also supported.
--
--   Usage:
--
--   @
--   assert $
--      for_all @(BvSort 8) $ \x ->
--          exists $ \y ->
--            x - y === 0
--   @
--
--   The lambdas 'y' is existentially quantified here.
--   It will only be scoped for the lambdas body.
exists :: forall t. KnownSMTSort t => (Expr t -> Expr BoolSort) -> Expr BoolSort
exists = Exists Nothing

-- | Select a value from an array.
select :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v
select = ArrSelect

-- | Store a value in an array.
store :: (KnownSMTSort k, KnownSMTSort v, Ord (HaskellType k)) => Expr (ArraySort k v) -> Expr k -> Expr v -> Expr (ArraySort k v)
store = ArrStore

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

toReal    :: Expr IntSort  -> Expr RealSort
toReal = ToReal

toInt     :: Expr RealSort -> Expr IntSort
toInt = ToInt

isInt     :: Expr RealSort -> Expr BoolSort
isInt = IsInt
