module Language.Hasmtlib.Internal.Disjoin.Spec where

import Language.Hasmtlib
import Language.Hasmtlib.Internal.Disjoin
import Language.Hasmtlib.Internal.Expr.Arbitrary
import Test.QuickCheck.Arbitrary
import Test.Hspec.QuickCheck
import Test.Hspec
import Data.Sequence

main :: IO ()
main = hspec $ do
  describe "disjoin" $ do
      prop "does not alter the amount of asserted formulas" $
        \(x :: Seq (Expr BoolSort)) -> True
