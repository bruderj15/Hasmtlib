module Language.Hasmtlib.Example.Bitvector where

import Language.Hasmtlib
import Control.Monad.IO.Class

main :: IO ()
main = do
  iSolver <- interactiveSolver optimathsat
  interactiveWith iSolver $ do
    setOption $ Custom "opt.priority" "lex"
    setOption $ ProduceModels True
    setLogic "QF_LIA"

    x <- var @IntSort

    assert $ x >? -2

    minimize x

    (_, sol) <- solve
    liftIO $ print $ decode sol x

    return ()
