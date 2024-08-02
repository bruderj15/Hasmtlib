{-# LANGUAGE LambdaCase #-}

module Language.Hasmtlib.Internal.Expr.Analyze where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.SMTSort
import Data.Coerce
import Data.Maybe
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import qualified Data.Vector.Sized as Vector

-- | For many expressions, returns an 'IntSet.IntSet' containing all underlying identifiers of 'SMTVar's occuring in any of the given formulas.
varIdsAll :: (Functor f, Foldable f, KnownSMTSort t) => f (Expr t) -> IntSet.IntSet
varIdsAll = IntSet.unions . fmap varIds1
{-# INLINEABLE varIdsAll #-}

-- | Given an expression returns an 'IntSet.IntSet' containing all underlying identifiers of 'SMTVar's occuring in that formula.
varIds1 :: KnownSMTSort t => Expr t -> IntSet.IntSet
varIds1 = foldr (\(SomeSMTSort v) -> IntSet.insert (coerce v)) mempty . vars1

-- | For many expressions, returns a 'Set.Set' containing all 'SMTVar's occuring in any of the given formulas.
varsAll :: (Foldable f, KnownSMTSort t) => f (Expr t) -> Set.Set (SomeKnownSMTSort SMTVar)
varsAll = foldr (\f vs -> vs <> vars1 f) mempty
{-# INLINEABLE varsAll #-}

-- | Given an expression returns an 'Set.Set' containing all 'SMTVar's occuring in that formula.
vars1 :: KnownSMTSort t => Expr t -> Set.Set (SomeKnownSMTSort SMTVar)
vars1 (Var v)            = Set.singleton $ SomeSMTSort v
vars1 (Constant _)       = mempty
vars1 (Plus x y)         = vars1 x <> vars1 y
vars1 (Neg x)            = vars1 x
vars1 (Mul x y)          = vars1 x <> vars1 y
vars1 (Abs x)            = vars1 x
vars1 (Mod x y)          = vars1 x <> vars1 y
vars1 (IDiv x y)         = vars1 x <> vars1 y
vars1 (Div x y)          = vars1 x <> vars1 y
vars1 (LTH x y)          = vars1 x <> vars1 y
vars1 (LTHE x y)         = vars1 x <> vars1 y
vars1 (EQU xs)           = Vector.foldl' (\vs x -> vs <> vars1 x) mempty xs
vars1 (Distinct xs)      = Vector.foldl' (\vs x -> vs <> vars1 x) mempty xs
vars1 (GTHE x y)         = vars1 x <> vars1 y
vars1 (GTH x y)          = vars1 x <> vars1 y
vars1 (Not x)            = vars1 x
vars1 (And x y)          = vars1 x <> vars1 y
vars1 (Or x y)           = vars1 x <> vars1 y
vars1 (Impl x y)         = vars1 x <> vars1 y
vars1 (Xor x y)          = vars1 x <> vars1 y
vars1 Pi                 = mempty
vars1 (Sqrt x)           = vars1 x
vars1 (Exp x)            = vars1 x
vars1 (Sin x)            = vars1 x
vars1 (Cos x)            = vars1 x
vars1 (Tan x)            = vars1 x
vars1 (Asin x)           = vars1 x
vars1 (Acos x)           = vars1 x
vars1 (Atan x)           = vars1 x
vars1 (ToReal x)         = vars1 x
vars1 (ToInt x)          = vars1 x
vars1 (IsInt x)          = vars1 x
vars1 (Ite p t f)        = vars1 p <> vars1 t <> vars1 f
vars1 (BvNot x)          = vars1 x
vars1 (BvAnd x y)        = vars1 x <> vars1 y
vars1 (BvOr x y)         = vars1 x <> vars1 y
vars1 (BvXor x y)        = vars1 x <> vars1 y
vars1 (BvNand x y)       = vars1 x <> vars1 y
vars1 (BvNor x y)        = vars1 x <> vars1 y
vars1 (BvNeg x)          = vars1 x
vars1 (BvAdd x y)        = vars1 x <> vars1 y
vars1 (BvSub x y)        = vars1 x <> vars1 y
vars1 (BvMul x y)        = vars1 x <> vars1 y
vars1 (BvuDiv x y)       = vars1 x <> vars1 y
vars1 (BvuRem x y)       = vars1 x <> vars1 y
vars1 (BvShL x y)        = vars1 x <> vars1 y
vars1 (BvLShR x y)       = vars1 x <> vars1 y
vars1 (BvConcat x y)     = vars1 x <> vars1 y
vars1 (BvRotL _ x)       = vars1 x
vars1 (BvRotR _ x)       = vars1 x
vars1 (BvuLT x y)        = vars1 x <> vars1 y
vars1 (BvuLTHE x y)      = vars1 x <> vars1 y
vars1 (BvuGTHE x y)      = vars1 x <> vars1 y
vars1 (BvuGT x y)        = vars1 x <> vars1 y
vars1 (ArrSelect i arr)  = vars1 i <> vars1 arr
vars1 (ArrStore i x arr) = vars1 i <> vars1 x <> vars1 arr
vars1 (ForAll mQv expr)  = vars1Q mQv expr
vars1 (Exists mQv expr)  = vars1Q mQv expr

vars1Q :: KnownSMTSort t => Maybe (SMTVar t) -> (Expr t -> Expr BoolSort) -> Set.Set (SomeKnownSMTSort SMTVar)
vars1Q mQv expr = Set.delete (SomeSMTSort qv) $ vars1 $ expr (Var qv)
  where
    qv = fromMaybe (coerce (-1 :: Int)) mQv
