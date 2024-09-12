module Language.Hasmtlib.Example.Arith where

import Prelude hiding ((&&))
import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith @SMT (solver $ debugging verbosely cvc5) $ do
    setLogic "QF_LIA"

    x <- var @IntSort

    let a1 = x + x
        a2 = a1 + a1

    assert $ a2 === a2

    setSharingMode StableNames
    assert $ a2 === a2

    setSharingMode None
    assert $ a2 === a2

    return x

  print res
