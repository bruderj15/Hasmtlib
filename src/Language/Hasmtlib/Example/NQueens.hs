{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Hasmtlib.Example.McCarthyArrayAxioms where

import Language.Hasmtlib
import Control.Monad
import Data.List (groupBy, sortOn, transpose)
import Data.Function (on)

main :: IO ()
main = do
  (res, sol) <- solveWith @SMT (solver z3) $ nqueens @IntSort 4
  case res of
    Sat -> case sol of
      Nothing -> putStrLn "No solution"
      Just board -> forM_ board print
    r -> print r

nqueens :: forall t s m. (MonadSMT s m, KnownSMTSort t, Num (Expr t), Orderable (Expr t)) => Int -> m [[Expr t]]
nqueens n = do
  setLogic $ case sortSing @t of
    SIntSort -> "QF_LIA"
    SRealSort -> "QF_LRA"
    SBvSort _ _ -> "QF_BV"
    _ -> "ALL"

  board <- replicateM n $ replicateM n var

  forM_ (concat board) $ assert . queenDomain
  forM_ board $ assert . ((=== 1) . sum)
  forM_ (transpose board) $ assert . ((=== 1) . sum)
  forM_ (diagonals board) $ assert . ((<=? 1) . sum)

  return board

queenDomain ::(Equatable (Expr t), Num (Expr t)) => Expr t -> Expr BoolSort
queenDomain f = (f === 0) `xor` (f === 1)

diagonals :: [[a]] -> [[a]]
diagonals mat = diagonals1 mat ++ diagonals2 mat

indexedMatrix :: [[a]] -> [((Int, Int), a)]
indexedMatrix mat = [((i, j), val) | (i, row) <- zip [0..] mat, (j, val) <- zip [0..] row]

diagonals1 :: [[a]] -> [[a]]
diagonals1 mat = map (map snd) . groupBy ((==) `on` (uncurry (-) . fst)) . sortOn (uncurry (-) . fst) $ indexedMatrix mat

diagonals2 :: [[a]] -> [[a]]
diagonals2 mat = map (map snd) . groupBy ((==) `on` (uncurry (+) . fst)) . sortOn (uncurry (+) . fst) $ indexedMatrix mat
