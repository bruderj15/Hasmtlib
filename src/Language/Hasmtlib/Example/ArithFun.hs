module Language.Hasmtlib.Example.ArithFun where

import Prelude hiding (mod, (&&))
import Language.Hasmtlib
import Control.Monad

main :: IO ()
main = do
  res <- solveWith @SMT (solver cvc5) $ do
    setLogic "QF_LIRA"

    xs <- replicateM 10 $ var @RealSort

    forM_ xs $ \x -> assert $ x >=? 0 && x <? fromIntegral (length xs)
    forM_ xs $ assert . isIntSort
    assert $ distinct xs

    return xs

  print res
