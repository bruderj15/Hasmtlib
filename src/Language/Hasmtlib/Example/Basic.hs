module Language.Hasmtlib.Example.Basic2 where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith cvc5Debug $ do
    setLogic "ALL"

    y <- var @RealType
    assert $ y <=? 1500000000000
    
    return y

  print res
