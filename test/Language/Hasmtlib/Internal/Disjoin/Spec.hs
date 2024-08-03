module Language.Hasmtlib.Internal.Disjoin.Spec where

import Language.Hasmtlib hiding ((&&))
import Language.Hasmtlib.Internal.Disjoin
import Language.Hasmtlib.Arbitrary
import Test.QuickCheck.Arbitrary
import Test.Hspec.QuickCheck
import Test.Hspec
import Data.Sequence as Seq
import Data.IntSet as IntSet
import Data.Foldable as Foldable
import Control.Monad
import Control.Lens

main :: IO ()
main = hspec $ do
  describe "disjoin" $ do
      prop "does not alter the amount of asserted formulas" $
        \(s :: SMT) -> Seq.length (s^.formulas) == Seq.length (disjoin s ^.folded.formulas)
      prop "does not alter the amount of variables" $
        \(s :: SMT) -> Seq.length (s^.vars) == Seq.length (disjoin s ^.folded.vars)
      prop "transforms a single SMT-state into many disjoint ones" $
        \(s :: SMT) -> fst $
            Foldable.foldr
              (\vs' (b, vs) -> (b && IntSet.disjoint vs vs', vs <> vs'))
              (True, mempty) $
              fmap
                (IntSet.fromList . Foldable.toList . fmap (\(SomeSMTSort (SMTVar i)) -> i) . view vars) $
                disjoin s
