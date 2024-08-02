module Language.Hasmtlib.Internal.Disjoin.Spec where

import Language.Hasmtlib
import Language.Hasmtlib.Internal.Disjoin
import Language.Hasmtlib.Arbitrary
import Test.QuickCheck.Arbitrary
import Test.Hspec.QuickCheck
import Test.Hspec
import Data.Sequence as Seq
import Control.Monad
import Control.Lens

main :: IO ()
main = hspec $ do
  describe "disjoin" $ do
      prop "does not alter the amount of asserted formulas" $
        \(s :: SMT) -> Seq.length (s^.formulas) == sum (Seq.length . view formulas <$> disjoin s)
      prop "does not alter the amount of variables" $
        \(s :: SMT) -> Seq.length (s^.vars) == sum (Seq.length . view vars <$> disjoin s)
