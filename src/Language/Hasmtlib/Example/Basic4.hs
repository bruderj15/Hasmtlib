module Language.Hasmtlib.Example.Basic3 where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith cvc5 $ do
    setLogic "ALL"

    x <- var @RealType

    assert $ x === pi
  
    return x

  case res of
    (Sat, Just x) -> print x
    _              -> putStrLn $ show res ++ " No solution"

  return ()
