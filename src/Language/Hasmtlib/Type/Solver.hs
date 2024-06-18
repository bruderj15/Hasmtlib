module Language.Hasmtlib.Type.Solver where

import Language.Hasmtlib.Type.Pipe
import Language.Hasmtlib.Type.Option
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Codec
import qualified SMTLIB.Backends as B
import qualified SMTLIB.Backends.Process as P
import Data.Default
import Control.Monad.State

-- | Data that can have a 'B.Solver'
class WithSolver a where
  withSolver :: B.Solver -> a

instance WithSolver Pipe where
  withSolver = Pipe 0 Nothing

-- | @'solveWith' solver prob@ solves a SMT problem @prob@ with the given
-- @solver@. It returns a pair consisting of:
--
-- 1. A 'Result' that indicates if @prob@ is satisfiable ('Sat'),
--    unsatisfiable ('Unsat'), or if the solver could not determine any
--    results ('Unknown').
--
-- 2. A 'Decoded' answer that was decoded using the solution to @prob@. Note
--    that this answer is only meaningful if the 'Result' is 'Sat' or 'Unknown' and
--    the answer value is in a 'Just'.
--
-- Here is a small example of how to use 'solveWith':
--
-- @
-- import Language.Hasmtlib
--
-- main :: IO ()
-- main = do
--   res <- solveWith (solver cvc5) $ do
--     setLogic "QF_LIA"
-- 
--     x <- var @IntSort
-- 
--     assert $ x >? 0
--     
--     return x
-- 
--   print res
-- @
solveWith :: (Monad m, Default s, Codec a) => Solver s m -> StateT s m a -> m (Result, Maybe (Decoded a))
solveWith solver m = do
  (a, problem) <- runStateT m def
  (result, solution) <- solver problem
    
  return (result, decode solution a)

-- | Pipes an SMT-problem interactively to the solver.
--   Enables incremental solving by default.
--   Here is a small example of how to use it for solving a problem utilizing the solvers incremental stack:
-- 
-- @
-- import Language.Hasmtlib
-- import Data.Proxy
-- import Control.Monad.IO.Class
-- 
-- main :: IO ()
-- main = do
--   cvc5Living <- interactiveSolver cvc5
--   interactiveWith cvc5Living $ do
--     setLogic "QF_LIA"
-- 
--     x <- var @IntSort
-- 
--     assert $ x >? 0
-- 
--     (res, sol) <- solve
--     liftIO $ print res
--     liftIO $ print $ decode sol x
-- 
--     push
--     y <- var @IntSort
-- 
--     assert $ y <? 0
--     assert $ x === y
-- 
--     res' <- checkSat
--     liftIO $ print res'
--     pop
-- 
--     res'' <- checkSat
--     liftIO $ print res''
-- 
--   return ()
-- @
interactiveWith :: (MonadIO m, WithSolver s) => (B.Solver, P.Handle) -> StateT s m () -> m ()
interactiveWith (solver, handle) m = do
   liftIO $ B.command_ solver $ render (Incremental True)
   _ <- runStateT m $ withSolver solver
   liftIO $ P.close handle