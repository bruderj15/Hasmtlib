module Language.Hasmtlib.Example.IncrementalOptimization where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith (solver @OMT z3) $ do
    setLogic "QF_LIA"

    x <- var @IntSort

    assert $ x >? -2
    assertSoftWeighted (x >? -1) 5.0

    minimize x

    return x

  print res
