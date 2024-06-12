module Language.Hasmtlib.Example.Arith where

import Prelude hiding (mod, (&&))
import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith cvc5 $ do
    setLogic "QF_LIA"

    x <- var @IntType
    y <- var @IntType

    assert $ y >? 0 && x /== y
    assert $ x `mod` 42 === y
    assert $ y + x + 1 >=? x + y
    
    return (x,y)

  print res