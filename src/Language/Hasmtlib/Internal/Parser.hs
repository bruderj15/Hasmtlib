{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.Hasmtlib.Internal.Parser where

import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Type.Expr
import Data.Ratio ((%))
import Data.Coerce
import qualified Data.IntMap as IM
import Data.Attoparsec.ByteString hiding (Result)
import Data.Attoparsec.ByteString.Char8 hiding (Result)
import Control.Applicative

answerParser :: Parser (Result, Solution)
answerParser = do
  result  <- resultParser
  _       <- (skipSpace >> char '(' >> skipSpace) <|> skipSpace
  varSols <- many $ parseSomeSol <* skipSpace
  _       <- (skipSpace >> char ')' >> skipSpace) <|> skipSpace

  let varSolsMap = IM.fromList $ fmap (\case someVarSol@(SomeKnownSMTRepr varSol) -> (coerce (smtVar varSol), someVarSol)) varSols
  
  return (result, varSolsMap)

resultParser :: Parser Result
resultParser = (string "sat" *> pure Sat)
           <|> (string "unsat" *> pure Unsat)
           <|> (string "unknown" *> pure Unknown)

parseBool :: Parser Bool
parseBool = (string "true" *> pure True) <|> (string "false" *> pure False)

parseRational :: Parser Rational
parseRational = do
  _           <- char '(' >> skipSpace >> char '/' >> skipSpace
  numerator   <- decimal
  _           <- skipSpace
  denominator <- decimal
  _           <- skipSpace >> char ')'

  return $ numerator % denominator

parseModel :: forall t. KnownSMTRepr t => Parser (ValueType t)
parseModel = do
  case singRepr @t of
    IntRepr  -> string "Int"  >> skipSpace >> decimal
    RealRepr -> string "Real" >> skipSpace >> fromRational <$> parseRational
    BoolRepr -> string "Bool" >> skipSpace >> parseBool

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

parseSomeSol :: Parser (SomeKnownSMTRepr SMTVarSol)
parseSomeSol = SomeKnownSMTRepr <$> (parseSol @IntType)
           <|> SomeKnownSMTRepr <$> (parseSol @RealType)
           <|> SomeKnownSMTRepr <$> (parseSol @BoolType)
