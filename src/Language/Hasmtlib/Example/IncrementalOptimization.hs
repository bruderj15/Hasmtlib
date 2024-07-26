module Language.Hasmtlib.Example.IncrementalOptimization where

import Language.Hasmtlib
import Control.Monad.IO.Class

main :: IO ()
main = do
  iSolver <- interactiveSolver z3
  interactiveWith @Pipe iSolver $ do
    setOption $ ProduceModels True
    setLogic "QF_LIA"

    x <- var @IntSort

    assert $ x >? -2
    assertSoftWeighted (x >? -1) 5.0

    minimize x

    (_, sol) <- solve
    liftIO $ print $ decode sol x

    return ()
