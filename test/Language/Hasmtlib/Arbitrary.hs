{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Arbitrary where

import Prelude hiding (not, and, or, (&&), (||), Integral(..))
import Language.Hasmtlib.Internal.Expr.Analyze
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Internal.Bitvec
import Language.Hasmtlib
import Test.QuickCheck hiding ((===), (==>))
import Data.Proxy
import Data.Coerce
import Data.Maybe
import qualified Data.Dependent.Map as DMap
import qualified Data.IntSet as IntSet
import qualified Data.Sequence as Seq
import Control.Monad
import GHC.TypeLits

instance KnownNat n => Arbitrary (Bitvec n) where
  arbitrary = arbitrary @Integer >>= return . fromInteger

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (ConstArray k v) where
  arbitrary = liftM2 ConstArray arbitrary arbitrary

instance (KnownSMTSort t, Arbitrary (HaskellType t)) => Arbitrary (Expr t) where
  arbitrary =
    let varMap =  DMap.singleton SBoolSort (SMTVarList [SMTVar i | i <- [1..10]]) `DMap.union`
                  DMap.singleton SIntSort  (SMTVarList [SMTVar i | i <- [11..20]]) `DMap.union`
                  DMap.singleton SRealSort  (SMTVarList [SMTVar i | i <- [21..30]]) `DMap.union`
                  DMap.singleton (SBvSort (Proxy @8))  (SMTVarList [SMTVar i | i <- [31..40]])
     in sized (sizedArbitraryExpr varMap)

newtype SMTVarList (t :: SMTSort) = SMTVarList { unList :: [SMTVar t] }

sizedArbitraryExpr :: forall t. (KnownSMTSort t, Arbitrary (HaskellType t)) => DMap.DMap SSMTSort SMTVarList -> Int -> Gen (Expr t)
sizedArbitraryExpr varMap 0 = oneof
  [
    constant <$> arbitrary
  , maybe (constant <$> arbitrary) (oneof . fmap (return . Var) . unList) $ DMap.lookup (sortSing @t) varMap
  ]
sizedArbitraryExpr varMap n = case sortSing @t of
  SBoolSort      -> oneof
    [
      liftM2 (<?)  (sizedArbitraryExpr @IntSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (<?)  (sizedArbitraryExpr @RealSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (<?)  (sizedArbitraryExpr @(BvSort 8) varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (<=?) (sizedArbitraryExpr @IntSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (<=?) (sizedArbitraryExpr @RealSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (<=?) (sizedArbitraryExpr @(BvSort 8) varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (===) (sizedArbitraryExpr @IntSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (===) (sizedArbitraryExpr @RealSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (===) (sizedArbitraryExpr @(BvSort 8) varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (/==) (sizedArbitraryExpr @IntSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (/==) (sizedArbitraryExpr @RealSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (/==) (sizedArbitraryExpr @(BvSort 8) varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (>=?) (sizedArbitraryExpr @IntSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (>=?) (sizedArbitraryExpr @RealSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (>=?) (sizedArbitraryExpr @(BvSort 8) varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (>?)  (sizedArbitraryExpr @IntSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (>?)  (sizedArbitraryExpr @RealSort varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (>?)  (sizedArbitraryExpr @(BvSort 8) varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , fmap   not   (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (&&)  (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (||)  (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 (==>) (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 xor   (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , fmap isIntSort (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM3 ite     (sizedArbitraryExpr @BoolSort varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 select  (sizedArbitraryExpr @(ArraySort BoolSort _) varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 select  (sizedArbitraryExpr @(ArraySort IntSort _) varMap    $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 select  (sizedArbitraryExpr @(ArraySort RealSort _) varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 select  (sizedArbitraryExpr @(ArraySort (BvSort 8) _) varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    ]
  SIntSort       -> oneof $
    numExprGens varMap n ++ [
      fmap toIntSort (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 mod (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 div (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    ]
  SRealSort      -> oneof $
    numExprGens varMap n ++ [
      liftM2 (/) (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , fmap toRealSort (sizedArbitraryExpr varMap $ n `div` 2)
    , fmap sqrt (sizedArbitraryExpr varMap $ n `div` 2)
    , fmap exp  (sizedArbitraryExpr varMap $ n `div` 2)
    , fmap sin  (sizedArbitraryExpr varMap $ n `div` 2)
    , fmap cos  (sizedArbitraryExpr varMap $ n `div` 2)
    , fmap tan  (sizedArbitraryExpr varMap $ n `div` 2)
    , fmap asin (sizedArbitraryExpr varMap $ n `div` 2)
    , fmap acos (sizedArbitraryExpr varMap $ n `div` 2)
    , fmap atan (sizedArbitraryExpr varMap $ n `div` 2)
    , return $ constant pi
    ]
  SBvSort _      -> oneof $
    numExprGens varMap n ++ [
      liftM2 mod (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    , liftM2 div (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
    ]
  SArraySort _ _ -> sizedArbitraryExpr varMap 0

numExprGens :: (Num (Expr t), KnownSMTSort t, Arbitrary (HaskellType t)) => DMap.DMap SSMTSort SMTVarList -> Int -> [Gen (Expr t)]
numExprGens varMap n =
  [
    fmap negate (sizedArbitraryExpr varMap $ n `div` 2)
  , liftM2 (+)  (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
  , liftM2 (-)  (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
  , liftM2 (*)  (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
  , liftM3 ite  (sizedArbitraryExpr @BoolSort varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
  , liftM2 select (sizedArbitraryExpr @(ArraySort BoolSort _) varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
  , liftM2 select (sizedArbitraryExpr @(ArraySort IntSort _) varMap    $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
  , liftM2 select (sizedArbitraryExpr @(ArraySort RealSort _) varMap   $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
  , liftM2 select (sizedArbitraryExpr @(ArraySort (BvSort 8) _) varMap $ n `div` 2) (sizedArbitraryExpr varMap $ n `div` 2)
  ]

instance Arbitrary SMT where
  arbitrary = do
    fs <- arbitrary
    let vs      = foldr (Seq.<|) mempty $ varsAll fs
        lastVar = fromMaybe 0 (IntSet.maxView (varIdsAll fs) >>= return . fst)
    return $ SMT lastVar vs fs Nothing mempty
