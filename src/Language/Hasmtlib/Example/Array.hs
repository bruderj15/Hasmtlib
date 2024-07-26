module Language.Hasmtlib.Example.Array where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith @SMT (solver cvc5) $ do
    setLogic "QF_AUFLIA"

    x <- var @(ArraySort IntSort BoolSort)
    let y = constant $ asConst false

    assert $ store y 1 true === x

    let x1 = select x 1

    return (x, x1, select x 100)

  print res
