{-# LANGUAGE LambdaCase #-}

module Language.Hasmtlib.Internal.Parser where

import Language.Hasmtlib.Type.Expr
import Language.Hasmtlib.Type.Solution
import Data.Coerce
import Data.Ratio ((%))
import Data.Attoparsec.ByteString hiding (Result)
import Data.Attoparsec.ByteString.Char8 hiding (Result)
import qualified Data.IntMap as IM
import Control.Applicative

answerParser :: Parser (Result, Solution)
answerParser = do
  result  <- resultParser
  model   <- modelParser
  
  return (result, model)

resultParser :: Parser Result
resultParser = (string "sat" *> pure Sat)
           <|> (string "unsat" *> pure Unsat)
           <|> (string "unknown" *> pure Unknown)

modelParser :: Parser Solution
modelParser = do
  _ <-  (skipSpace >> char '(' >> skipSpace >> string "model" >> skipSpace)    -- Mathsat
    <|> (skipSpace >> char '(' >> skipSpace)                                   -- Standard (CVC5, Z3)
    <|> skipSpace                                                              -- answer does not contain a model      
  varSols <- many $ parseSomeSol <* skipSpace
  _       <- (skipSpace >> char ')' >> skipSpace)
         <|> skipSpace

  return $ IM.fromList $ fmap (\case someVarSol@(SomeKnownSMTRepr varSol) -> (coerce (smtVar varSol), someVarSol)) varSols

parseSomeSol :: Parser (SomeKnownSMTRepr SMTVarSol)
parseSomeSol = SomeKnownSMTRepr <$> (parseSol @IntType)
           <|> SomeKnownSMTRepr <$> (parseSol @RealType)
           <|> SomeKnownSMTRepr <$> (parseSol @BoolType)

parseSol :: forall t. KnownSMTRepr t => Parser (SMTVarSol t)
parseSol = do
  _     <- char '(' >> skipSpace
  _     <- string "define-fun" >> skipSpace
  _     <- string "var_"
  varId <- decimal @Int
  _     <- skipSpace >> string "()" >> skipSpace
  value <- parseModel @t
  _     <- skipSpace >> char ')'

  return $ SMTVarSol (coerce varId) (putValue value)

parseModel :: forall t. KnownSMTRepr t => Parser (ValueType t)
parseModel = do
  case singRepr @t of
    IntRepr  -> string "Int"  >> skipSpace >> decimal
    RealRepr -> do
      _ <- string "Real"
      _ <- skipSpace
      (fromRational <$> parseRatioRational) <|> parseToRealRational <|> rational
    BoolRepr -> string "Bool" >> skipSpace >> parseBool

parseBool :: Parser Bool
parseBool = (string "true" *> pure True) <|> (string "false" *> pure False)

parseRatioRational :: Parser Rational
parseRatioRational = do
  _           <- char '(' >> skipSpace >> char '/' >> skipSpace
  numerator   <- decimal
  _           <- skipSpace
  denominator <- decimal
  _           <- skipSpace >> char ')'

  return $ numerator % denominator
  
parseToRealRational :: Parser Double
parseToRealRational = do
  _   <- char '(' >> skipSpace >> string "to_real" >> skipSpace
  dec <- decimal
  _   <- skipSpace >> char ')'
  
  return $ fromInteger dec
