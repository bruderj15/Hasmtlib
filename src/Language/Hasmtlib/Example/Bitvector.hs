module Language.Hasmtlib.Example.Bitvector where

import Language.Hasmtlib
import Data.Default

main :: IO ()
main = do
  res <- solveWith @SMT (debug bitwuzla def) $ do
    setLogic "QF_BV"

    xbv8 <- variable
    ybv8 <- var @(BvSort 8)

    assert $ true === (xbv8 `xor` ybv8)
    assert $ xbv8 <=? maxBound

    assert $ xbv8 >? 0
    assert $ ybv8 >? 0

    assert $ xbv8 + ybv8 >? xbv8 * ybv8

    return (xbv8, ybv8)

  print res
