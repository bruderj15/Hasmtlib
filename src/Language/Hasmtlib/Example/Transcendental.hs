module Language.Hasmtlib.Example.Transcendental where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith @SMT (solver $ debugging verbosely cvc5) $ do
    setLogic "ALL"

    x <- var @RealSort

    assert $ 0 === sin x
    assert $ x >? 0

    return x

  print res
