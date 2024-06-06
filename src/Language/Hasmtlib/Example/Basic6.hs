module Language.Hasmtlib.Example.Basic6 where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith yicesDebug $ do
    setLogic "QF_BV"

    mvar <- var @(BvType 8)

    assert $ mvar === maxBound

    return mvar

  print res
