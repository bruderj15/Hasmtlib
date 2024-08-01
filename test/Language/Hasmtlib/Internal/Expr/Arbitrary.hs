{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Internal.Expr.Arbitrary where

import Language.Hasmtlib
import Test.QuickCheck
import Data.Proxy
import Data.Coerce

instance Arbitrary (Expr BoolSort) where
  arbitrary = _
