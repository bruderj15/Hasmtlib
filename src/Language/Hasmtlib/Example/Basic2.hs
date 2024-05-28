module Language.Hasmtlib.Example.Basic2 where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith cvc5 $ do
    setLogic "ALL"

    x <- var @RealType

    assert $ x >? sin 3
  
    return x

  print res

  return ()
