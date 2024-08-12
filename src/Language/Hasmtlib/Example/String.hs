module Language.Hasmtlib.Example.Arith where

import Prelude hiding ((&&))
import Language.Hasmtlib
import Control.Lens

main :: IO ()
main = do
  res <- solveWith @SMT (solver cvc5) $ do
    setLogic "QF_S"

    x <- var @StringSort
    y <- var @StringSort
    z <- var @StringSort

    assert $ x === "H" <> "e" <> "llo"
    assert $ y === "World"
    assert $ z === x <> " " <> y

    p <- var @StringSort
    let argh :: Expr StringSort = "Argh!"
    assert $ p === "_Cons: " <| z <> argh^.ix 4

    return p

  print res
