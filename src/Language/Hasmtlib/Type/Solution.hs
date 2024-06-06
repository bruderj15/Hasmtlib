module Language.Hasmtlib.Type.Solution where

import Language.Hasmtlib.Type.Expr
import Data.IntMap

-- | Function that turns a state (SMT) into a result and a solution
type Solver s m = s -> m (Result, Solution)

data Result = Unsat | Unknown | Sat deriving (Show, Eq, Ord)

-- | Map from varId to some solution
type Solution = IntMap (SomeKnownSMTRepr SMTVarSol)

-- | A solution for a single variable
data SMTVarSol (t :: SMTType) = SMTVarSol { smtVar :: SMTVar t, val :: Value t } deriving (Show, Eq, Ord)



