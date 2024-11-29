{- |
This module provides debugging capabilites for the problem definition and communication with the external solver.
-}
module Language.Hasmtlib.Type.Debugger
  (
    -- * Type
    Debugger(..), StateDebugger(..)

    -- * Construction
    -- ** Volume
  , silently
  , noisy
  , verbosely

    -- ** Information
  , optionish
  , logicish
  , varish
  , assertionish
  , incrementalStackish, getValueish
  , responseish
  )
where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.OMT
import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.SMTSort
import Data.Some.Constraint
import Data.Sequence as Seq hiding ((|>))
import Data.ByteString.Lazy (ByteString, split)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as ByteString.Char8
import Data.Default
import Data.Functor.Contravariant
import Control.Lens hiding (op)

-- | A type holding actions for debugging states holding SMT-Problems.
data Debugger s = Debugger
  { debugState          :: s -> IO ()               -- ^ Debug the entire state
  , debugOption         :: Builder -> IO ()
  , debugLogic          :: Builder -> IO ()
  , debugVar            :: Builder -> IO ()
  , debugAssert         :: Builder -> IO ()
  , debugPush           :: Builder -> IO ()
  , debugPop            :: Builder -> IO ()
  , debugCheckSat       :: Builder -> IO ()
  , debugGetModel       :: Builder -> IO ()
  , debugGetValue       :: Builder -> IO ()
  , debugMinimize       :: Builder -> IO ()
  , debugMaximize       :: Builder -> IO ()
  , debugAssertSoft     :: Builder -> IO ()
  , debugResultResponse :: ByteString -> IO ()      -- ^ Debug the solvers raw response for @(check-sat)@
  , debugModelResponse  :: ByteString -> IO ()      -- ^ Debug the solvers raw response for @(get-model)@
  }

instance Default (Debugger s) where
  def = verbosely

-- | Concats actions
instance Semigroup (Debugger s) where
  x <> y = Debugger
    (\input -> debugState x input          >> debugState y input)
    (\input -> debugOption x input         >> debugOption y input)
    (\input -> debugLogic x input          >> debugLogic y input)
    (\input -> debugVar x input            >> debugVar y input)
    (\input -> debugAssert x input         >> debugAssert y input)
    (\input -> debugPop x input            >> debugPop y input)
    (\input -> debugCheckSat x input       >> debugCheckSat y input)
    (\input -> debugGetModel x input       >> debugGetModel y input)
    (\input -> debugGetValue x input       >> debugGetValue y input)
    (\input -> debugMinimize x input       >> debugMinimize y input)
    (\input -> debugMaximize x input       >> debugMaximize y input)
    (\input -> debugMaximize x input       >> debugMaximize y input)
    (\input -> debugAssertSoft x input     >> debugAssertSoft y input)
    (\input -> debugResultResponse x input >> debugResultResponse y input)
    (\input -> debugModelResponse x input  >> debugModelResponse y input)

instance Monoid (Debugger s) where
  mempty = silently

instance Contravariant Debugger where
  contramap f' debugger = debugger { debugState = f . f' }
    where
      f = debugState debugger

printer :: Builder -> IO ()
printer = ByteString.Char8.putStrLn . toLazyByteString

-- | The silent 'Debugger'. Does not debug at all.
silently :: Debugger s
silently = Debugger
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)
  (const mempty)

-- | The noisy 'Debugger'.
--
--   Debugs the entire problem definition.
noisy :: Debugger s
noisy = Debugger
  (const mempty)
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  (const mempty)
  (const mempty)

-- | The verbose 'Debugger'.
--
--   Debugs all communication between Haskell and the external solver.
verbosely :: Debugger s
verbosely = Debugger
  (const mempty)
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  printer
  (ByteString.Char8.putStrLn . (\s -> "\n" <> s <> "\n"))
  (mapM_ (putStrLn . toString) . split 13)

-- | A 'Debugger' for debugging all rendered options that have been set.
optionish :: Debugger s
optionish = silently { debugOption = printer }

-- | A 'Debugger' for debugging the logic that has been set.
logicish :: Debugger s
logicish = silently { debugLogic = printer }

-- | A 'Debugger' for debugging all variable declarations.
varish :: Debugger s
varish = silently { debugVar = printer }

-- | A 'Debugger' for debugging all assertions.
assertionish :: Debugger s
assertionish = silently { debugAssert = printer, debugAssertSoft = printer }

-- | A 'Debugger' for debugging every push/pop-interaction with the solvers incremental stack.
incrementalStackish :: Debugger s
incrementalStackish = silently { debugPush = printer, debugPop = printer }

-- | A 'Debugger' for debugging every @(get-value)@ call to the solver.
getValueish :: Debugger s
getValueish = silently { debugGetValue = printer }

-- | A 'Debugger' for debugging the entire and raw responses of a solver for the commands @(check-sat)@ and @(get-model)@.
responseish :: Debugger s
responseish = silently
  { debugResultResponse = ByteString.Char8.putStrLn . (\s -> "\n" <> s <> "\n")
  , debugModelResponse = mapM_ (putStrLn . toString) . split 13
  }

-- | A class that allows debugging states.
class StateDebugger s where
  -- | Debugs information about the problem like the amount of variables and assertions.
  statistically :: Debugger s

instance StateDebugger SMT where
  statistically = silently
    { debugState = \s -> do
      putStrLn $ "Bool Vars:  " ++ show (Seq.length $ Seq.filter (\(Some1 v) -> case sortSing' v of SBoolSort -> True ; _ -> False) $ s^.vars)
      putStrLn $ "Arith Vars: " ++ show (Seq.length $ Seq.filter (\(Some1 v) -> case sortSing' v of SBoolSort -> False ; _ -> True) $ s^.vars)
      putStrLn $ "Assertions: " ++ show (Seq.length (s^.formulas))
      putStrLn $ "Size:       " ++ show (sum $ fmap exprSize $ s^.formulas)
    }

instance StateDebugger OMT where
  statistically = silently
    { debugState = \omt -> do
      putStrLn $ "Bool Vars:       " ++ show (Seq.length $ Seq.filter (\(Some1 v) -> case sortSing' v of SBoolSort -> True ; _ -> False) $ omt^.smt.vars)
      putStrLn $ "Arith Vars:      " ++ show (Seq.length $ Seq.filter (\(Some1 v) -> case sortSing' v of SBoolSort -> False ; _ -> True) $ omt^.smt.vars)
      putStrLn $ "Hard assertions: " ++ show (Seq.length (omt^.smt.formulas))
      putStrLn $ "Soft assertions: " ++ show (Seq.length (omt^.softFormulas))
      putStrLn $ "Optimizations:   " ++ show (Seq.length (omt^.targetMinimize) + Seq.length (omt^.targetMaximize))
      let omtSize = sum (fmap exprSize $ omt^.smt.formulas)
                  + sum (fmap (exprSize . view formula) $ omt^.softFormulas)
                  + sum (fmap (\(Some1 (Minimize expr)) -> exprSize expr) $ omt^.targetMinimize)
                  + sum (fmap (\(Some1 (Maximize expr)) -> exprSize expr) $ omt^.targetMaximize)
      putStrLn $ "Size:            " ++ show omtSize
    }
