module Language.Hasmtlib.Internal.Disjoin where

import Language.Hasmtlib.Internal.Expr.Analyze
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.SMT
import Data.Dependent.Map as DMap hiding (mapMaybe)
import Data.IntMap as IntMap hiding (mapMaybe)
import Data.IntSet as IntSet
import Data.Sequence as Seq
import Data.Maybe
import Data.Coerce
import qualified Data.Foldable as Foldable
import Control.Lens hiding ((|>))

class Disjoinable a where
  disjoin :: a -> Seq a

merge :: Foldable f => f Solution -> Solution
merge = unionsWithKey (\_ (IntValueMap l) (IntValueMap r) -> IntValueMap $ l `IntMap.union` r) . Foldable.toList

instance Disjoinable SMT where
  disjoin smt =
    fmap (\fs -> smt
        & vars %~ Seq.filter (\(SomeSMTSort v) -> varIdsAll fs ^. contains (coerce v))
        & formulas .~ fs) $
    fst $
    Foldable.foldr'
      (\f (fss, v_fssIndex) ->
        let vs = IntSet.toList $ varIds1 f
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
                                (mempty :: Seq (Seq (Expr BoolSort)), pure f)
                                fss
                        f_index      = Seq.length fss'
                        v_fssIndex'  = IntSet.foldr' (\v -> at v ?~ f_index) v_fssIndex $ varIdsAll mergedFs
                    in (fss' |> mergedFs, v_fssIndex')
      )
      (mempty :: Seq (Seq (Expr t)), mempty :: IntMap Int)
      (smt^.formulas)
