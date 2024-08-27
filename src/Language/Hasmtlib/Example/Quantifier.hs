module Language.Hasmtlib.Example.Quantifier where

import Prelude hiding (mod, (&&), (||))
import Language.Hasmtlib
import Data.Default

main :: IO ()
main = do
  res <- solveWith @SMT (debug cvc5 def) $ do
    setLogic "BV"

    z <- var @(BvSort Signed 8)

    assert $ z === 0

    assert $
      for_all $ \x ->
          exists $ \y ->
            x - y === z

    return ()

  print res
