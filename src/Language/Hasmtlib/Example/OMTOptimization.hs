module Language.Hasmtlib.Example.IncrementalOptimization where

import Prelude hiding ((&&))
import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith @OMT (solver $ debugging verbosely z3) $ do
    setLogic "QF_LIA"

    x <- var @IntSort
    y <- var @IntSort

    assert $ x <? 10 && y <? 5 && (y <? 7 ==> x === 1)

    maximize $ x + y

    return (x,y)

  print res
