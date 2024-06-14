module Language.Hasmtlib.Example.Quantifier where

import Prelude hiding (mod, (&&), (||))
import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith cvc5Debug $ do
    setLogic "BV"

    z <- var @(BvType 8)

    assert $ z === 0

    assert $
      for_all $ \x ->
          exists $ \y ->
            x - y === z

    return ()

  print res