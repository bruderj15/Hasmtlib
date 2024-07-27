module Language.Hasmtlib.Internal.Disjoin where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.SMT
import Data.Dependent.Map as DMap hiding (mapMaybe)
import Data.IntMap as IntMap hiding (mapMaybe)
import Data.IntSet as IntSet
import Data.Sequence as Seq
import qualified Data.Vector.Sized as Vector
import Data.Maybe
import Data.Coerce
import qualified Data.Foldable as Foldable
import Control.Lens hiding ((|>))

merge :: Foldable f => f Solution -> Solution
merge = unionsWithKey (\_ (IntValueMap l) (IntValueMap r) -> IntValueMap $ l `IntMap.union` r) . Foldable.toList

disjoin :: SMT -> Seq SMT
disjoin smt =
  fmap (\fs -> smt
      & vars %~ Seq.filter (\(SomeSMTSort v) -> allVarIds fs ^. contains (coerce v))
      & formulas .~ fs) $
  fst $
  Foldable.foldr'
    (\f (fss, v_fssIndex) ->
      let vs = IntSet.toList $ varIds f
          fssIndexs = mapMaybe (v_fssIndex IntMap.!?) vs
       in case fssIndexs of
            [] -> let f_index = Seq.length fss
                      fss' = fss |> pure f
                      v_fssIndex' = v_fssIndex <> IntMap.fromList (fmap (, f_index) vs)
                   in (fss', v_fssIndex')
            is -> let (fss', mergedFs) = ifoldl'
                              (\i (fss'', merged) fs ->
                                if i `elem` is then (fss'', merged >< fs) else (fss'' |> fs, merged)
                              )
                              (mempty :: Seq (Seq (Expr BoolSort)), mempty :: Seq (Expr BoolSort))
                              fss
                      f_index      = Seq.length fss'
                      v_fssIndex'  = IntSet.foldr' (\v -> at v ?~ f_index) v_fssIndex $ allVarIds mergedFs
                   in (fss' |> mergedFs, v_fssIndex')
    )
    (mempty :: Seq (Seq (Expr t)), mempty :: IntMap Int)
    (smt^.formulas)

allVarIds :: (Functor f, Foldable f) => f (Expr t) -> IntSet
allVarIds = IntSet.unions . fmap varIds

varIds :: Expr t -> IntSet
varIds (Var var)          = IntSet.singleton $ coerce var
varIds (Constant _)       = mempty
varIds (Plus x y)         = varIds x `IntSet.union` varIds y
varIds (Neg x)            = varIds x
varIds (Mul x y)          = varIds x `IntSet.union` varIds y
varIds (Abs x)            = varIds x
varIds (Mod x y)          = varIds x `IntSet.union` varIds y
varIds (IDiv x y)         = varIds x `IntSet.union` varIds y
varIds (Div x y)          = varIds x `IntSet.union` varIds y
varIds (LTH x y)          = varIds x `IntSet.union` varIds y
varIds (LTHE x y)         = varIds x `IntSet.union` varIds y
varIds (EQU xs)           = Vector.foldl' (\vs x -> vs `IntSet.union` varIds x) mempty xs
varIds (Distinct xs)      = Vector.foldl' (\vs x -> vs `IntSet.union` varIds x) mempty xs
varIds (GTHE x y)         = varIds x `IntSet.union` varIds y
varIds (GTH x y)          = varIds x `IntSet.union` varIds y
varIds (Not x)            = varIds x
varIds (And x y)          = varIds x `IntSet.union` varIds y
varIds (Or x y)           = varIds x `IntSet.union` varIds y
varIds (Impl x y)         = varIds x `IntSet.union` varIds y
varIds (Xor x y)          = varIds x `IntSet.union` varIds y
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
varIds (Ite p t f)        = varIds p `IntSet.union` varIds t `IntSet.union` varIds f
varIds (BvNot x)          = varIds x
varIds (BvAnd x y)        = varIds x `IntSet.union` varIds y
varIds (BvOr x y)         = varIds x `IntSet.union` varIds y
varIds (BvXor x y)        = varIds x `IntSet.union` varIds y
varIds (BvNand x y)       = varIds x `IntSet.union` varIds y
varIds (BvNor x y)        = varIds x `IntSet.union` varIds y
varIds (BvNeg x)          = varIds x
varIds (BvAdd x y)        = varIds x `IntSet.union` varIds y
varIds (BvSub x y)        = varIds x `IntSet.union` varIds y
varIds (BvMul x y)        = varIds x `IntSet.union` varIds y
varIds (BvuDiv x y)       = varIds x `IntSet.union` varIds y
varIds (BvuRem x y)       = varIds x `IntSet.union` varIds y
varIds (BvShL x y)        = varIds x `IntSet.union` varIds y
varIds (BvLShR x y)       = varIds x `IntSet.union` varIds y
varIds (BvConcat x y)     = varIds x `IntSet.union` varIds y
varIds (BvRotL _ x)       = varIds x
varIds (BvRotR _ x)       = varIds x
varIds (BvuLT x y)        = varIds x `IntSet.union` varIds y
varIds (BvuLTHE x y)      = varIds x `IntSet.union` varIds y
varIds (BvuGTHE x y)      = varIds x `IntSet.union` varIds y
varIds (BvuGT x y)        = varIds x `IntSet.union` varIds y
varIds (ArrSelect i arr)  = varIds i `IntSet.union` varIds arr
varIds (ArrStore i x arr) = varIds i `IntSet.union` varIds x `IntSet.union` varIds arr
varIds (ForAll _ _)       = mempty
varIds (Exists _ _)       = mempty
