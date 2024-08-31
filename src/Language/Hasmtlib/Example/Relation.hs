module Language.Hasmtlib.Example.Relation where

import Prelude hiding (not)
import Language.Hasmtlib
import Control.Lens

main :: IO ()
main = do
  (Sat, Just rel') <- solveWith @SMT (solver opensmt) $ do
    setLogic "QF_LIA"

    rel <- relation ((0,0), (4,4))
    assert $ atLeast @IntSort 7 $ elems rel
    assertMaybe $ do
      item <- rel^?ix (0,0)
      return $ item === false

    return rel

  putStrLn $ table rel'
