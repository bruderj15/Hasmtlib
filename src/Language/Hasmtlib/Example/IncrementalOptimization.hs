module Language.Hasmtlib.Example.IncrementalOptimization where

import Language.Hasmtlib
import Control.Monad

main :: IO ()
main = do
  res <- interactiveWith (debugging verbosely z3) $ do
    setOption $ ProduceModels True
    setLogic "QF_LIA"

    x <- var @IntSort

    assert $ x <? 10
    assert $ x >? 0

    (_, sol) <- solveMaximized x (Just (+2)) Nothing
    return $ decode sol x
  print $ join res
