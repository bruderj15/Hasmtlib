{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Language.Hasmtlib.Type.Solution where

import Language.Hasmtlib.Type.Expr
import Data.IntMap as IMap hiding (foldl)
import Data.Dependent.Map as DMap
import Data.Dependent.Map.Lens
import Control.Lens

-- | Function that turns a state into a result and a solution.
type Solver s m = s -> m (Result, Solution)

-- | Results of check-sat commands.
data Result = Unsat | Unknown | Sat deriving (Show, Eq, Ord)

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

-- This is very ugly with the SomeKnownSMTSort in ...Internal.Expr already
-- Surely theres abstraction possible, but not worth the bloat currently
data SomeKnownOrdSMTSort f where
  -- The Ord (HaskellType t) seems off here
  -- It is - but we need to to parse ArraySorts existentially where Ord needs to hold for the HaskellType of Key-SMTSort
  -- Composing constraints bloats the code too much
  -- The Ord (HaskellType t) is not a problem though as long as all rhs of the type-family hold it, which is trivial
  SomeKnownOrdSMTSort :: forall (t :: SMTSort) f. (KnownSMTSort t, Ord (HaskellType t)) => f t -> SomeKnownOrdSMTSort f

-- | Create a 'Solution' from some 'SMTVarSol's.
fromSomeVarSols :: [SomeKnownOrdSMTSort SMTVarSol] -> Solution
fromSomeVarSols = foldl
  (\dsol (SomeKnownOrdSMTSort s) -> let sSort = sortSing' s in
    dsol & dmat sSort %~
      (\case
        Nothing -> Just $ IntValueMap $ IMap.singleton (s^.solVar.varId) (s^.solVal)
        Just (IntValueMap im) -> Just $ IntValueMap $ IMap.insert (s^.solVar.varId) (s^.solVal) im
      )
  )
  mempty