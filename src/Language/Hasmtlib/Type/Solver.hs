module Language.Hasmtlib.Type.Solver where

import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Codec
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
--     return (x,x,x,x)
-- 
--   print res
solveWith :: (Monad m, Default s, Codec a) => Solver s m -> StateT s m a -> m (Result, Maybe (Decoded a))
solveWith solve m = do
  (a, problem) <- runStateT m def
  (result, solution) <- solve problem
    
  return (result, decode solution a)
  
