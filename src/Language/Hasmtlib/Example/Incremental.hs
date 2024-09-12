module Language.Hasmtlib.Example.Incremental where

import Language.Hasmtlib
import Control.Monad.IO.Class

main :: IO ()
main = do
  a <- interactiveWith z3 $ do
    setOption $ ProduceModels True
    setLogic "QF_LIA"

    x <- var @IntSort

    assert $ x >? 0

    (res, sol) <- solve
    liftIO $ print res
    liftIO $ print $ decode sol x

    push
    y <- var @IntSort

    assert $ y <? 0
    assert $ x === y

    res' <- checkSat
    liftIO $ print res'
    pop

    res'' <- checkSat
    liftIO $ print res''

    return res''

  print a
