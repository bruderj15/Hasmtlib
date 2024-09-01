module Language.Hasmtlib.Example.Relation where

import Prelude hiding (not)
import Language.Hasmtlib
import Control.Lens
import Control.Monad

main :: IO ()
main = do
  (Sat, Just rel') <- solveWith @SMT (solver opensmt) $ do
    setLogic "QF_LIA"

    rel <- relation ((0,0), (4,4))

    forM_ (image rel <$> domain rel) (assert . exactly @IntSort 1)
    forM_ (preimage rel <$> domain rel) (assert . exactly @IntSort 1)

    assertMaybe $ do
      item <- rel^?ix (0,0)
      return $ item === true

    return rel

  putStrLn $ table rel'
