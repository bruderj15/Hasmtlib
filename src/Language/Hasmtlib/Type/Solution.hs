{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Type.Solution where

import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Type.SMTSort
import Data.IntMap as IMap hiding (foldl)
import Data.Dependent.Map as DMap
import Data.Dependent.Map.Lens
import Control.Lens

-- | Function that turns a state into a result and a solution.
type Solver s m = s -> m (Result, Solution)

-- | Results of check-sat commands.
data Result = Unsat | Unknown | Sat deriving (Show, Eq, Ord)

instance Semigroup Result where
  Unsat <> Unsat     = Unsat
  Unsat <> Unknown   = Unsat
  Unsat <> Sat       = Unsat
  Unknown <> Unsat   = Unsat
  Unknown <> Unknown = Unknown
  Unknown <> Sat     = Unknown
  Sat <> Unsat       = Unsat
  Sat <> Unknown     = Unknown
  Sat <> Sat         = Sat

instance Monoid Result where
  mempty = Sat
  mappend = (<>)

-- | A Solution is a dependent map 'DMap' from 'SSMTSort's t to 'IntMap' t.
type Solution = DMap SSMTSort IntValueMap

-- | Newtype for 'IntMap' 'Value' so we can use it as right-hand-side of 'DMap'.
newtype IntValueMap t = IntValueMap (IntMap (Value t))
  deriving stock Show
  deriving newtype (Semigroup, Monoid)

-- | A solution for a single variable.
data SMTVarSol (t :: SMTSort) = SMTVarSol
  { _solVar :: SMTVar t                       -- ^ A variable in the SMT-Problem
  , _solVal :: Value t                        -- ^ An assignment for this variable in a solution
  } deriving Show
$(makeLenses ''SMTVarSol)

-- | Alias class for constraint 'Ord' ('HaskellType' t)
class Ord (HaskellType t) => OrdHaskellType t
instance Ord (HaskellType t) => OrdHaskellType t

-- | An existential wrapper that hides some known 'SMTSort' with an 'Ord' 'HaskellType'
type SomeKnownOrdSMTSort f = SomeSMTSort '[KnownSMTSort, OrdHaskellType] f

-- | Create a 'Solution' from some 'SMTVarSol's.
fromSomeVarSols :: [SomeKnownOrdSMTSort SMTVarSol] -> Solution
fromSomeVarSols = foldl
  (\dsol (SomeSMTSort s) -> let sSort = sortSing' s in
    dsol & dmat sSort %~
      (\case
        Nothing -> Just $ IntValueMap $ IMap.singleton (s^.solVar.varId) (s^.solVal)
        Just (IntValueMap im) -> Just $ IntValueMap $ IMap.insert (s^.solVar.varId) (s^.solVal) im
      )
  )
  mempty
