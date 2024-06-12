module Language.Hasmtlib.Example.Incremental where

import Language.Hasmtlib
import Control.Monad.IO.Class

main :: IO ()
main = do
  cvc5Living <- cvc5Alive
  interactive cvc5Living $ do
    setLogic "QF_LIA"

    x <- var @IntType

    assert $ x >? 0

    (res, sol) <- solve
    liftIO $ print res
    liftIO $ print $ decode sol x

    push
    y <- var @IntType

    assert $ y <? 0
    assert $ x === y

    res' <- checkSat
    liftIO $ print res'
    pop

    res'' <- checkSat
    liftIO $ print res''

  return ()