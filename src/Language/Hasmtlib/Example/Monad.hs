module Language.Hasmtlib.Example.Monad where

import Language.Hasmtlib
import Control.Monad

main :: IO ()
main = do
  res <- solveWith (solver @SMT cvc5) $ do
    setLogic "QF_LIA"

    xs <- replicateM 100 $ var @IntSort

    forM_ (zip xs [1 :: Integer .. ]) $ \(x, i) -> assert $ x === fromInteger i

    return xs

  case res of
    (Sat, Just xs) -> print xs
    _              -> putStrLn $ show res ++ " No solution"

  return ()
