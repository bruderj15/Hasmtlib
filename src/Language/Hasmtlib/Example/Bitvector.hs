module Language.Hasmtlib.Example.Bitvector where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith yicesDebug $ do
    setLogic "QF_BV"

    xbv8 <- variable
    ybv8 <- var @(BvType 8)

    assert $ true === (xbv8 `xor` ybv8)
    assert $ xbv8 <=? maxBound

    assert $ xbv8 >? 0
    assert $ ybv8 >? 0

    assert $ xbv8 + ybv8 >? xbv8 * ybv8

    return (xbv8, ybv8)

  print res
