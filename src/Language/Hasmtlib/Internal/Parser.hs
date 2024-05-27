{-# LANGUAGE OverloadedStrings #-}

module Language.Hasmtlib.Internal.Parser where

import Language.Hasmtlib.Type.Solution
import Language.Hasmtlib.Type.Expr
import Prelude hiding (putStr)
import Data.Ratio ((%))
import Data.Coerce
import Data.Attoparsec.ByteString hiding (Result)
import Data.Attoparsec.ByteString.Char8 hiding (Result)
import Data.Sequence (fromList)
import Control.Applicative

answerParser :: Parser (Result, Solution)
answerParser = do
  result  <- resultParser
  _       <- skipSpace
  varSols <- many parseSomeSol

  return (result, fromList varSols)

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
  _     <- skipSpace
  _     <- string "()" >> skipSpace
  value <- parseModel @t
  _     <- char ')'

  return $ SMTVarSol (coerce varId) (putValue value)

parseSomeSol :: Parser (SomeKnownSMTRepr SMTVarSol)
parseSomeSol = SomeKnownSMTRepr <$> (parseSol @IntType)
           <|> SomeKnownSMTRepr <$> (parseSol @RealType)
           <|> SomeKnownSMTRepr <$> (parseSol @BoolType)
