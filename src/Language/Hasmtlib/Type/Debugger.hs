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
import Data.Sequence as Seq hiding ((|>), filter)
import Data.ByteString.Lazy hiding (singleton)
import Data.ByteString.Lazy.UTF8 (toString)
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as ByteString.Char8
import Data.Default
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
      putStrLn $ "Variables:  " ++ show (Seq.length (s^.vars))
      putStrLn $ "Assertions: " ++ show (Seq.length (s^.formulas))
    }

instance StateDebugger OMT where
  statistically = silently
    { debugState = \omt -> do
      putStrLn $ "Variables:       " ++ show (Seq.length (omt^.smt.vars))
      putStrLn $ "Hard assertions: " ++ show (Seq.length (omt^.smt.formulas))
      putStrLn $ "Soft assertions: " ++ show (Seq.length (omt^.softFormulas))
      putStrLn $ "Optimizations:   " ++ show (Seq.length (omt^.targetMinimize) + Seq.length (omt^.targetMaximize))
    }
