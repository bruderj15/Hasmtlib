module Language.Hasmtlib.Example.Counting where

import Language.Hasmtlib
import Control.Monad

main :: IO ()
main = do
  res <- solveWith (solver cvc5) $ do
    setLogic "QF_LIA"

    xs <- replicateM 10 $ var @BoolSort

    -- Type information necessary here because we need it internally for addition
    assert $ atMost  (2 :: Expr IntSort) xs
    assert $ atLeast @IntSort 1 xs

    return xs

  print res
