module Language.Hasmtlib.Internal.Expr.Analyze where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.SMTSort
import Data.Coerce
import Data.Maybe
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq
import qualified Data.Vector.Sized as Vector

-- | For many expressions, returns an 'IntSet.IntSet' containing all underlying identifiers 'SMTVar' occuring in any of the given formulas.
varIdsAll :: (Functor f, Foldable f) => f (Expr t) -> IntSet.IntSet
varIdsAll = IntSet.unions . fmap varIds1

-- | Given an expression returns an 'IntSet.IntSet' containing all underlying identifiers of 'SMTVar's occuring in that formula.
varIds1 :: Expr t -> IntSet.IntSet
varIds1 (Var v)          = IntSet.singleton $ coerce v
varIds1 (Constant _)       = mempty
varIds1 (Plus x y)         = varIds1 x `IntSet.union` varIds1 y
varIds1 (Neg x)            = varIds1 x
varIds1 (Mul x y)          = varIds1 x `IntSet.union` varIds1 y
varIds1 (Abs x)            = varIds1 x
varIds1 (Mod x y)          = varIds1 x `IntSet.union` varIds1 y
varIds1 (IDiv x y)         = varIds1 x `IntSet.union` varIds1 y
varIds1 (Div x y)          = varIds1 x `IntSet.union` varIds1 y
varIds1 (LTH x y)          = varIds1 x `IntSet.union` varIds1 y
varIds1 (LTHE x y)         = varIds1 x `IntSet.union` varIds1 y
varIds1 (EQU xs)           = Vector.foldl' (\vs x -> vs `IntSet.union` varIds1 x) mempty xs
varIds1 (Distinct xs)      = Vector.foldl' (\vs x -> vs `IntSet.union` varIds1 x) mempty xs
varIds1 (GTHE x y)         = varIds1 x `IntSet.union` varIds1 y
varIds1 (GTH x y)          = varIds1 x `IntSet.union` varIds1 y
varIds1 (Not x)            = varIds1 x
varIds1 (And x y)          = varIds1 x `IntSet.union` varIds1 y
varIds1 (Or x y)           = varIds1 x `IntSet.union` varIds1 y
varIds1 (Impl x y)         = varIds1 x `IntSet.union` varIds1 y
varIds1 (Xor x y)          = varIds1 x `IntSet.union` varIds1 y
varIds1 Pi                 = mempty
varIds1 (Sqrt x)           = varIds1 x
varIds1 (Exp x)            = varIds1 x
varIds1 (Sin x)            = varIds1 x
varIds1 (Cos x)            = varIds1 x
varIds1 (Tan x)            = varIds1 x
varIds1 (Asin x)           = varIds1 x
varIds1 (Acos x)           = varIds1 x
varIds1 (Atan x)           = varIds1 x
varIds1 (ToReal x)         = varIds1 x
varIds1 (ToInt x)          = varIds1 x
varIds1 (IsInt x)          = varIds1 x
varIds1 (Ite p t f)        = varIds1 p `IntSet.union` varIds1 t `IntSet.union` varIds1 f
varIds1 (BvNot x)          = varIds1 x
varIds1 (BvAnd x y)        = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvOr x y)         = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvXor x y)        = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvNand x y)       = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvNor x y)        = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvNeg x)          = varIds1 x
varIds1 (BvAdd x y)        = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvSub x y)        = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvMul x y)        = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvuDiv x y)       = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvuRem x y)       = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvShL x y)        = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvLShR x y)       = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvConcat x y)     = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvRotL _ x)       = varIds1 x
varIds1 (BvRotR _ x)       = varIds1 x
varIds1 (BvuLT x y)        = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvuLTHE x y)      = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvuGTHE x y)      = varIds1 x `IntSet.union` varIds1 y
varIds1 (BvuGT x y)        = varIds1 x `IntSet.union` varIds1 y
varIds1 (ArrSelect i arr)  = varIds1 i `IntSet.union` varIds1 arr
varIds1 (ArrStore i x arr) = varIds1 i `IntSet.union` varIds1 x `IntSet.union` varIds1 arr
varIds1 (ForAll _ _)       = mempty
varIds1 (Exists _ _)       = mempty

varsAll :: (Functor f, Foldable f, KnownSMTSort t) => f (Expr t) -> Seq.Seq (SomeKnownSMTSort SMTVar)
varsAll = foldr nubAppend mempty . fmap vars1

vars1 :: KnownSMTSort t => Expr t -> Seq.Seq (SomeKnownSMTSort SMTVar)
vars1 (Var v)            = Seq.singleton $ SomeSMTSort v
vars1 (Constant _)       = mempty
vars1 (Plus x y)         = vars1 x `nubAppend` vars1 y
vars1 (Neg x)            = vars1 x
vars1 (Mul x y)          = vars1 x `nubAppend` vars1 y
vars1 (Abs x)            = vars1 x
vars1 (Mod x y)          = vars1 x `nubAppend` vars1 y
vars1 (IDiv x y)         = vars1 x `nubAppend` vars1 y
vars1 (Div x y)          = vars1 x `nubAppend` vars1 y
vars1 (LTH x y)          = vars1 x `nubAppend` vars1 y
vars1 (LTHE x y)         = vars1 x `nubAppend` vars1 y
vars1 (EQU xs)           = Vector.foldl' (\vs x -> vs `nubAppend` vars1 x) mempty xs
vars1 (Distinct xs)      = Vector.foldl' (\vs x -> vs `nubAppend` vars1 x) mempty xs
vars1 (GTHE x y)         = vars1 x `nubAppend` vars1 y
vars1 (GTH x y)          = vars1 x `nubAppend` vars1 y
vars1 (Not x)            = vars1 x
vars1 (And x y)          = vars1 x `nubAppend` vars1 y
vars1 (Or x y)           = vars1 x `nubAppend` vars1 y
vars1 (Impl x y)         = vars1 x `nubAppend` vars1 y
vars1 (Xor x y)          = vars1 x `nubAppend` vars1 y
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
vars1 (Ite p t f)        = vars1 p `nubAppend` vars1 t `nubAppend` vars1 f
vars1 (BvNot x)          = vars1 x
vars1 (BvAnd x y)        = vars1 x `nubAppend` vars1 y
vars1 (BvOr x y)         = vars1 x `nubAppend` vars1 y
vars1 (BvXor x y)        = vars1 x `nubAppend` vars1 y
vars1 (BvNand x y)       = vars1 x `nubAppend` vars1 y
vars1 (BvNor x y)        = vars1 x `nubAppend` vars1 y
vars1 (BvNeg x)          = vars1 x
vars1 (BvAdd x y)        = vars1 x `nubAppend` vars1 y
vars1 (BvSub x y)        = vars1 x `nubAppend` vars1 y
vars1 (BvMul x y)        = vars1 x `nubAppend` vars1 y
vars1 (BvuDiv x y)       = vars1 x `nubAppend` vars1 y
vars1 (BvuRem x y)       = vars1 x `nubAppend` vars1 y
vars1 (BvShL x y)        = vars1 x `nubAppend` vars1 y
vars1 (BvLShR x y)       = vars1 x `nubAppend` vars1 y
vars1 (BvConcat x y)     = vars1 x `nubAppend` vars1 y
vars1 (BvRotL _ x)       = vars1 x
vars1 (BvRotR _ x)       = vars1 x
vars1 (BvuLT x y)        = vars1 x `nubAppend` vars1 y
vars1 (BvuLTHE x y)      = vars1 x `nubAppend` vars1 y
vars1 (BvuGTHE x y)      = vars1 x `nubAppend` vars1 y
vars1 (BvuGT x y)        = vars1 x `nubAppend` vars1 y
vars1 (ArrSelect i arr)  = vars1 i `nubAppend` vars1 arr
vars1 (ArrStore i x arr) = vars1 i `nubAppend` vars1 x `nubAppend` vars1 arr
vars1 (ForAll _ _)       = mempty
vars1 (Exists _ _)       = mempty

nubAppend :: Eq a => Seq.Seq a -> Seq.Seq a -> Seq.Seq a
nubAppend xs ys = foldr (\x zs -> if isJust $ x `Seq.elemIndexL` zs then zs else zs Seq.|> x) ys xs
