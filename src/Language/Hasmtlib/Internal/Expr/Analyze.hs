module Language.Hasmtlib.Internal.Expr.Analyze where

import Language.Hasmtlib.Internal.Expr
import Data.IntSet
import Data.Coerce
import qualified Data.Vector.Sized as Vector

-- | For many expressions, returns an 'IntSet' containing all underlying identifiers 'SMTVar' occuring in any of the given formulas.
allVarIds :: (Functor f, Foldable f) => f (Expr t) -> IntSet
allVarIds = unions . fmap varIds

-- | Given an expression returns an 'IntSet' containing all underlying identifiers 'SMTVar' occuring in that formula.
varIds :: Expr t -> IntSet
varIds (Var var)          = singleton $ coerce var
varIds (Constant _)       = mempty
varIds (Plus x y)         = varIds x `union` varIds y
varIds (Neg x)            = varIds x
varIds (Mul x y)          = varIds x `union` varIds y
varIds (Abs x)            = varIds x
varIds (Mod x y)          = varIds x `union` varIds y
varIds (IDiv x y)         = varIds x `union` varIds y
varIds (Div x y)          = varIds x `union` varIds y
varIds (LTH x y)          = varIds x `union` varIds y
varIds (LTHE x y)         = varIds x `union` varIds y
varIds (EQU xs)           = Vector.foldl' (\vs x -> vs `union` varIds x) mempty xs
varIds (Distinct xs)      = Vector.foldl' (\vs x -> vs `union` varIds x) mempty xs
varIds (GTHE x y)         = varIds x `union` varIds y
varIds (GTH x y)          = varIds x `union` varIds y
varIds (Not x)            = varIds x
varIds (And x y)          = varIds x `union` varIds y
varIds (Or x y)           = varIds x `union` varIds y
varIds (Impl x y)         = varIds x `union` varIds y
varIds (Xor x y)          = varIds x `union` varIds y
varIds Pi                 = mempty
varIds (Sqrt x)           = varIds x
varIds (Exp x)            = varIds x
varIds (Sin x)            = varIds x
varIds (Cos x)            = varIds x
varIds (Tan x)            = varIds x
varIds (Asin x)           = varIds x
varIds (Acos x)           = varIds x
varIds (Atan x)           = varIds x
varIds (ToReal x)         = varIds x
varIds (ToInt x)          = varIds x
varIds (IsInt x)          = varIds x
varIds (Ite p t f)        = varIds p `union` varIds t `union` varIds f
varIds (BvNot x)          = varIds x
varIds (BvAnd x y)        = varIds x `union` varIds y
varIds (BvOr x y)         = varIds x `union` varIds y
varIds (BvXor x y)        = varIds x `union` varIds y
varIds (BvNand x y)       = varIds x `union` varIds y
varIds (BvNor x y)        = varIds x `union` varIds y
varIds (BvNeg x)          = varIds x
varIds (BvAdd x y)        = varIds x `union` varIds y
varIds (BvSub x y)        = varIds x `union` varIds y
varIds (BvMul x y)        = varIds x `union` varIds y
varIds (BvuDiv x y)       = varIds x `union` varIds y
varIds (BvuRem x y)       = varIds x `union` varIds y
varIds (BvShL x y)        = varIds x `union` varIds y
varIds (BvLShR x y)       = varIds x `union` varIds y
varIds (BvConcat x y)     = varIds x `union` varIds y
varIds (BvRotL _ x)       = varIds x
varIds (BvRotR _ x)       = varIds x
varIds (BvuLT x y)        = varIds x `union` varIds y
varIds (BvuLTHE x y)      = varIds x `union` varIds y
varIds (BvuGTHE x y)      = varIds x `union` varIds y
varIds (BvuGT x y)        = varIds x `union` varIds y
varIds (ArrSelect i arr)  = varIds i `union` varIds arr
varIds (ArrStore i x arr) = varIds i `union` varIds x `union` varIds arr
varIds (ForAll _ _)       = mempty
varIds (Exists _ _)       = mempty
