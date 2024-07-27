module Language.Hasmtlib.Example.DisjoinParallel where

import Prelude hiding (mod, (&&), putStrLn)
import Language.Hasmtlib
import Language.Hasmtlib.Internal.Disjoin
import Language.Hasmtlib.Internal.Render
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8
import Control.Monad

mySolver :: Solver SMT IO
mySolver s = do
  let smts = disjoin s
  forM_ (fmap renderSeq smts) $ mapM_ (putStrLn . toLazyByteString)

  return (Unknown, mempty)

main :: IO ()
main = do
  res <- solveWith @SMT mySolver $ do
    setLogic "QF_LIA"

    x <- var @IntSort
    y <- var @IntSort
    z <- var @IntSort

    assert $ x >? 0
    assert $ y >? 5

    assert $ z >? y && z >? x

    return (x,y)

  print res
