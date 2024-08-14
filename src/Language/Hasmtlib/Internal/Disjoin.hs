module Language.Hasmtlib.Internal.Disjoin
  ( Disjoinable(..)
  , merge
  )
where

import Language.Hasmtlib.Internal.Expr.Analyze
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Type.SMTSort
import Language.Hasmtlib.Type.SMT
import Data.Dependent.Map as DMap hiding (mapMaybe)
import Data.IntMap as IntMap hiding (mapMaybe)
import Data.IntSet as IntSet
import Data.Sequence as Seq hiding ((|>), (<|))
import Data.Maybe
import Data.Coerce
import Data.STRef
import qualified Data.Foldable as Foldable
import Control.Lens
import Control.Monad.ST
import Control.Monad

-- | Class that allows to split a datum into a sequence of disjoint data.
class Disjoinable a where
  -- | Disjoin a datum into a disjoint sequence of data.
  disjoin :: a -> Seq a

instance Disjoinable SMT where
  disjoin smt =
    (\fs -> smt
        & vars %~ Seq.filter (\(SomeSMTSort v) -> varIdsAll fs ^. contains (coerce v))
        & formulas .~ fs)
    <$> disjoinST smt

disjoinST :: SMT -> Seq (Seq (Expr BoolSort))
disjoinST s = runST $ do
  vId_fId_Ref <- newSTRef @(IntMap Int) mempty
  fId_fs_Ref  <- newSTRef @(IntMap (Seq (Expr BoolSort))) mempty

  iforM_ (s^.formulas) $ \formulaId f -> do
    vId_fId <- readSTRef vId_fId_Ref
    let vs = IntSet.toList $ varIds1 f
        fIds = mapMaybe (vId_fId IntMap.!?) vs
     in case fIds of
          [] -> do
            modifySTRef' fId_fs_Ref (at formulaId ?~ pure f)
            modifySTRef' vId_fId_Ref (<> IntMap.fromList (fmap (, formulaId) vs))
          _ -> do
            fId_fs <- readSTRef fId_fs_Ref
            let mergedFs =  f <| join (Seq.fromList $ mapMaybe (fId_fs IntMap.!?) fIds)
            writeSTRef fId_fs_Ref $ Prelude.foldr IntMap.delete fId_fs fIds -- delete old formula associations
            modifySTRef' fId_fs_Ref (at formulaId ?~ mergedFs)
            modifySTRef' vId_fId_Ref (IntMap.fromList (fmap (, formulaId) $ IntSet.toList $ varIdsAll mergedFs) <>) -- update new var associations

  fId_fs <- readSTRef fId_fs_Ref
  return $ Seq.fromList $ IntMap.elems fId_fs

-- | Merge many 'Solution's into a single one.
merge :: Foldable f => f Solution -> Solution
merge = unionsWithKey (\_ (IntValueMap l) (IntValueMap r) -> IntValueMap $ l `IntMap.union` r) . Foldable.toList
