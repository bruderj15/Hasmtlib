module Language.Hasmtlib.Example.Bitvector where

import Language.Hasmtlib
import Data.Default
import Data.Bits

main :: IO ()
main = do
  res <- solveWith @SMT (debug bitwuzla def) $ do
    setLogic "QF_BV"

    x <- var @(BvSort 8)

    assert $ x === clearBit (maxBound `div` 2) 2

    return x

  print res
