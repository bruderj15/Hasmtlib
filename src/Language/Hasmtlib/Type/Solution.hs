module Language.Hasmtlib.Type.Solution where

import Language.Hasmtlib.Type.Expr
import Data.IntMap

type Solver s m = s -> m (Result, Solution)

data Result = Unsat | Unknown | Sat deriving (Show, Eq, Ord)

type Solution = IntMap (SomeKnownSMTRepr SMTVarSol)

data SMTVarSol (t :: SMTType) = SMTVarSol { smtVar :: SMTVar t, val :: Value t } deriving (Show, Eq, Ord)



