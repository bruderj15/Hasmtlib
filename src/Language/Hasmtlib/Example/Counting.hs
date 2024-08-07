module Language.Hasmtlib.Example.Counting where

import Language.Hasmtlib
import Control.Monad

main :: IO ()
main = do
  res <- solveWith @SMT (solver cvc5) $ do
    setLogic "QF_LIA"

    xs <- replicateM 10 $ var @BoolSort

    -- Type information necessary here because we need it internally for addition
    assert $ atMost  (2 :: Expr IntSort) xs
    assert $ atLeast @IntSort 1 xs

    assert $ count @IntSort xs === 1 -- equivalent to: exactly 1

    return xs

  print res
