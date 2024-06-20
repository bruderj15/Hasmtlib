{-# LANGUAGE TemplateHaskell #-}

module Language.Hasmtlib.Type.Solution where

import Language.Hasmtlib.Type.Expr
import Data.IntMap
import Control.Lens

-- | Function that turns a state into a result and a solution.
type Solver s m = s -> m (Result, Solution)

-- | Results of check-sat commands.
data Result = Unsat | Unknown | Sat deriving (Show, Eq, Ord)

-- | A Solution is a Map from the variable-identifier to some solution for it.
type Solution = IntMap (SomeKnownSMTSort SMTVarSol)

-- | A solution for a single variable.
data SMTVarSol (t :: SMTSort) = SMTVarSol 
  { _solVar :: SMTVar t                       -- ^ A variable in the SMT-Problem
  , _solVal :: Value t                        -- ^ An assignment for this variable in a solution
  } deriving Show
$(makeLenses ''SMTVarSol)
