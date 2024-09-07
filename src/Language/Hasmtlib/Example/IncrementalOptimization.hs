module Language.Hasmtlib.Example.IncrementalOptimization where

import Language.Hasmtlib
import Control.Monad.IO.Class

main :: IO ()
main = do
  iz3 <- interactiveSolver z3
  debugInteractiveWith @Pipe iz3 $ do
    setOption $ ProduceModels True
    setLogic "QF_LIA"

    x <- var @IntSort

    assert $ x <? 10
    assert $ x >? 0

    (res, sol) <- solveMaximized x (Just (+2)) Nothing
    liftIO $ print res
    liftIO $ print $ decode sol x
