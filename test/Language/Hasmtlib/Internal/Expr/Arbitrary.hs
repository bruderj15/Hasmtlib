{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Language.Hasmtlib.Internal.Expr.Arbitrary where

import Prelude hiding (not, and, or, (&&), (||), Integral(..))
import Language.Hasmtlib
import Language.Hasmtlib.Internal.Bitvec
import Test.QuickCheck hiding ((===), (==>))
import Data.Proxy
import Data.Coerce
import Control.Monad
import GHC.TypeLits

instance KnownNat n => Arbitrary (Bitvec n) where
  arbitrary = arbitrary @Integer >>= return . fromInteger

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (ConstArray k v) where
  arbitrary = liftM2 ConstArray arbitrary arbitrary

instance (KnownSMTSort t, Arbitrary (HaskellType t)) => Arbitrary (Expr t) where
  arbitrary = sized sizedArbitraryExpr

sizedArbitraryExpr :: forall t. (KnownSMTSort t, Arbitrary (HaskellType t)) => Int -> Gen (Expr t)
sizedArbitraryExpr 0 = constant <$> arbitrary   -- TODO: Or Var
sizedArbitraryExpr n = case sortSing @t of
  SBoolSort      -> oneof
    [
      liftM2 (<?)  (sizedArbitraryExpr @IntSort     $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (<?)  (sizedArbitraryExpr @RealSort    $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (<?)  (sizedArbitraryExpr @(BvSort 8)  $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (<=?) (sizedArbitraryExpr @IntSort    $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (<=?) (sizedArbitraryExpr @RealSort   $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (<=?) (sizedArbitraryExpr @(BvSort 8) $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (===) (sizedArbitraryExpr @IntSort    $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (===) (sizedArbitraryExpr @RealSort   $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (===) (sizedArbitraryExpr @(BvSort 8) $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (/==) (sizedArbitraryExpr @IntSort    $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (/==) (sizedArbitraryExpr @RealSort   $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (/==) (sizedArbitraryExpr @(BvSort 8) $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (>=?) (sizedArbitraryExpr @IntSort    $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (>=?) (sizedArbitraryExpr @RealSort   $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (>=?) (sizedArbitraryExpr @(BvSort 8) $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (>?)  (sizedArbitraryExpr @IntSort     $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (>?)  (sizedArbitraryExpr @RealSort    $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (>?)  (sizedArbitraryExpr @(BvSort 8)  $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , fmap   not   (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (&&)  (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (||)  (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 (==>) (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 xor   (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , fmap isIntSort (sizedArbitraryExpr $ n `div` 2)
    , liftM3 ite     (sizedArbitraryExpr @BoolSort $ n `div` 2) (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 select  (sizedArbitraryExpr @(ArraySort BoolSort _)   $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 select  (sizedArbitraryExpr @(ArraySort IntSort _)    $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 select  (sizedArbitraryExpr @(ArraySort RealSort _)   $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 select  (sizedArbitraryExpr @(ArraySort (BvSort 8) _) $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    ]
  SIntSort       -> oneof $
    numExprGens n ++ [
      fmap toIntSort (sizedArbitraryExpr $ n `div` 2)
    , liftM2 mod (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 div (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    ]
  SRealSort      -> oneof $
    numExprGens n ++ [
      liftM2 (/) (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , fmap toRealSort (sizedArbitraryExpr $ n `div` 2)
    , fmap sqrt (sizedArbitraryExpr $ n `div` 2)
    , fmap exp  (sizedArbitraryExpr $ n `div` 2)
    , fmap sin  (sizedArbitraryExpr $ n `div` 2)
    , fmap cos  (sizedArbitraryExpr $ n `div` 2)
    , fmap tan  (sizedArbitraryExpr $ n `div` 2)
    , fmap asin (sizedArbitraryExpr $ n `div` 2)
    , fmap acos (sizedArbitraryExpr $ n `div` 2)
    , fmap atan (sizedArbitraryExpr $ n `div` 2)
    , return $ constant pi
    ]
  SBvSort _      -> oneof $
    numExprGens n ++ [
      liftM2 mod (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    , liftM2 div (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
    ]
  SArraySort _ _ -> sizedArbitraryExpr 0

numExprGens :: (Num (Expr t), KnownSMTSort t, Arbitrary (HaskellType t)) => Int -> [Gen (Expr t)]
numExprGens n =
  [
    fmap negate (sizedArbitraryExpr $ n `div` 2)
  , liftM2 (+)  (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
  , liftM2 (-)  (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
  , liftM2 (*)  (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
  , liftM3 ite  (sizedArbitraryExpr @BoolSort $ n `div` 2) (sizedArbitraryExpr $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
  , liftM2 select (sizedArbitraryExpr @(ArraySort BoolSort _)   $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
  , liftM2 select (sizedArbitraryExpr @(ArraySort IntSort _)    $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
  , liftM2 select (sizedArbitraryExpr @(ArraySort RealSort _)   $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
  , liftM2 select (sizedArbitraryExpr @(ArraySort (BvSort 8) _) $ n `div` 2) (sizedArbitraryExpr $ n `div` 2)
  ]
