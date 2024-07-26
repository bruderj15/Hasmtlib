module Language.Hasmtlib.Example.Transcendental where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith @SMT (solver cvc5) $ do
    setLogic "ALL"

    x <- var @RealSort

    assert $ x >? sin 3

    return x

  print res

  return ()
