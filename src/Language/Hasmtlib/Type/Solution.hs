{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
This module provides the types 'Solution' and 'Result'.

External SMT-Solvers responses are parsed into these types.
-}
module Language.Hasmtlib.Type.Solution
(
  -- * Result
  Result(..)

  -- * Solution
, Solution
, OrdHaskellType
, SomeKnownOrdSMTSort
, fromSomeVarSols

  -- ** IntValueMap
, IntValueMap(..)

  -- ** SMTVarVol
  -- *** Type
, SMTVarSol(..)

  -- *** Lens
, solVar, solVal

)
where

import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.Value
import Language.Hasmtlib.Type.SMTSort
import Data.IntMap as IMap hiding (foldl)
import Data.Dependent.Map as DMap
import Data.Dependent.Map.Lens
import Data.Some.Constraint
import Control.Lens

-- | Results of check-sat commands.
data Result = Unsat | Unknown | Sat deriving (Show, Eq, Ord)

type Solution = DMap SSMTSort IntValueMap

-- | Newtype for 'IntMap' 'Value' so we can use it as right-hand-side of 'DMap'.
newtype IntValueMap t = IntValueMap (IntMap (Value t))
  deriving newtype (Semigroup, Monoid)

deriving stock instance Show (Value t) => Show (IntValueMap t)

-- | A solution for a single variable.
data SMTVarSol (t :: SMTSort) = SMTVarSol
  { _solVar :: SMTVar t                       -- ^ A variable in the SMT-Problem
  , _solVal :: Value t                        -- ^ An assignment for this variable in a solution
  }
$(makeLenses ''SMTVarSol)
deriving stock instance Show (Value t) => Show (SMTVarSol t)

-- | Alias class for constraint 'Ord' ('HaskellType' t)
class Ord (HaskellType t) => OrdHaskellType t
instance Ord (HaskellType t) => OrdHaskellType t

-- | An existential wrapper that hides some known 'SMTSort' with an 'Ord' 'HaskellType'
type SomeKnownOrdSMTSort f = Somes1 '[(~) f] '[KnownSMTSort, OrdHaskellType]

-- | Create a 'Solution' from some 'SMTVarSol's.
fromSomeVarSols :: [SomeKnownOrdSMTSort SMTVarSol] -> Solution
fromSomeVarSols = foldl
  (\dsol (Some1 s) -> let sSort = sortSing' s in
    dsol & dmat sSort %~
      (\case
        Nothing -> Just $ IntValueMap $ IMap.singleton (s^.solVar.varId) (s^.solVal)
        Just (IntValueMap im) -> Just $ IntValueMap $ IMap.insert (s^.solVar.varId) (s^.solVal) im
      )
  )
  mempty
