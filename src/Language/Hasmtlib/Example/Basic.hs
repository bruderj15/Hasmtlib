module Language.Hasmtlib.Example.Basic2 where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith cvc5Debug $ do
    setLogic "QF_LRA"

    x <- var @RealType
    y <- var @RealType
    b <- var @BoolType

    assert $ x === min' @(Expr BoolType) 42 100
    assert $ b === true
    assert $ b ==> (y === x)

    return (x, y, b)

  print res

  return ()
