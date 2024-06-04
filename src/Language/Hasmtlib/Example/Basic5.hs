module Language.Hasmtlib.Example.Basic5 where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith cvc5Debug $ do
    setLogic "ALL"

    mvar <- variable @(Maybe (Expr RealType))

    mapM_ (assert . (=== 10)) mvar

    return mvar

  print res
