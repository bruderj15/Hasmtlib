module Language.Hasmtlib.Type.Solution where

import Language.Hasmtlib.Type.Expr
import Data.Dependent.Map

-- | Function that turns a state into a result and a solution.
type Solver s m = s -> m (Result, Solution)

-- | Results of check-sat commands.
data Result = Unsat | Unknown | Sat deriving (Show, Eq, Ord)

-- TODO
-- | A Solution is a Map from the variable-identifier to some solution for it. 
type Solution = DMap SMTVar Value 
