module Language.Hasmtlib.Type.Solver where

import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Codec
import Data.Default
import Control.Monad.State

solveWith :: (Monad m, Default s, Codec a) => Solver s m -> StateT s m a -> m (Result, Maybe (Decoded a))
solveWith solve m = do
  (a, problem) <- runStateT m def
  (result, solution) <- solve problem
    
  return (result, decode solution a)
  
