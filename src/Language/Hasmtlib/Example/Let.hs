module Language.Hasmtlib.Example.Let where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith @SMT (solver $ debugging assertionish cvc5) $ do
    setLogic "QF_LIA"

    x <- var @IntSort

    assert $
      let_ (x + x + x)
       in_ $ \l -> l === l + l

    return x

  print res
