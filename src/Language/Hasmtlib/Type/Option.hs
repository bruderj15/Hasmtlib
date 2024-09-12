{- |
This module provides the data-type 'SMTOption' for adjusting a SMT-Solvers options.
-}
module Language.Hasmtlib.Type.Option where

import Data.Data (Data)

-- | Options for SMT-Solvers.
data SMTOption =
    PrintSuccess  Bool              -- ^ Print \"success\" after each operation
  | ProduceModels Bool              -- ^ Produce a satisfying assignment after each successful checkSat
  | Incremental   Bool              -- ^ Incremental solving
  | Custom String String            -- ^ Custom options. First String is the option, second its value.
  deriving (Show, Eq, Ord, Data)
