{-# LANGUAGE BlockArguments #-}

module Language.Hasmtlib.Example.McCarthyArrayAxioms where

import Language.Hasmtlib

main :: IO ()
main = do
  res <- solveWith @SMT (solver cvc5) $ do
    setLogic "AUFLIRA"

    assert $
      for_all @(ArraySort RealSort RealSort) \arr ->
         for_all \i ->
            for_all \x ->
               select (store arr i x) i === x

    assert $
      for_all @(ArraySort IntSort BoolSort) \arr ->
         for_all \i ->
           for_all \j ->
             for_all \x ->
               i /== j ==> (select (store arr i x) j === select arr j)

    return ()

  print res
