module Language.Hasmtlib.Example.Transcendental where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith (solver cvc5) $ do
    setLogic "ALL"

    x <- var @RealSort

    assert $ x >? sin 3
  
    return x

  print res

  return ()
