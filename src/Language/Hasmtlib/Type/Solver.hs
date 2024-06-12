module Language.Hasmtlib.Type.Solver where

import Language.Hasmtlib.Type.SMT
import Language.Hasmtlib.Type.Pipe
import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Codec
import qualified SMTLIB.Backends as B
import qualified SMTLIB.Backends.Process as P
import Data.Default
import Control.Monad.State

-- | @'solveWith' solver prob@ solves a SAT problem @prob@ with the given
-- @solver@. It returns a pair consisting of:
--
-- 1. A 'Result' that indicates if @prob@ is satisfiable ('Satisfied'),
--    unsatisfiable ('Unsatisfied'), or if the solver could not determine any
--    results ('Unsolved').
--
-- 2. A 'Decoded' answer that was decoded using the solution to @prob@. Note
--    that this answer is only meaningful if the 'Result' is 'Satisfied' and
--    the answer value is in a 'Just'.
--
-- Here is a small example of how to use 'solveWith':
--
-- import Language.Hasmtlib
--
-- main :: IO ()
-- main = do
--   res <- solveWith cvc5 $ do
--     setLogic "QF_LIA"
-- 
--     x <- var @IntType
-- 
--     assert $ x >? 0
--     
--     return x
-- 
--   print res
solveWith :: (Monad m, Default s, Codec a) => Solver s m -> StateT s m a -> m (Result, Maybe (Decoded a))
solveWith solver m = do
  (a, problem) <- runStateT m def
  (result, solution) <- solver problem
    
  return (result, decode solution a)

-- | Pipes an SMT-problem incrementally to the solver.
--   Here is a small example of how to use it for solving a problem utilizing thr solvers incremental stack:
-- 
-- import Language.Hasmtlib
-- import Data.Proxy
-- import Control.Monad.IO.Class
-- 
-- main :: IO ()
-- main = do
--   cvc5Living <- cvc5Alive
--   interactive cvc5Living $ do
--     setLogic "QF_LIA"
-- 
--     x <- var @IntType
-- 
--     assert $ x >? 0
-- 
--     (res, sol) <- solve
--     liftIO $ print res
--     liftIO $ print $ decode sol x
-- 
--     push
--     y <- var @IntType
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
interactive :: MonadIO m => (B.Solver, P.Handle) -> StateT Pipe m () -> m ()
interactive (solver, handle) m = do
   liftIO $ B.command_ solver $ renderSMTLib2 (Incremental True)
   _ <- runStateT m $ withSolver solver
   liftIO $ P.close handle