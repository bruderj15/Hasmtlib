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
  model   <- anyModelParser
  
  return (result, model)

resultParser :: Parser Result
resultParser = (string "sat" *> pure Sat)
           <|> (string "unsat" *> pure Unsat)
           <|> (string "unknown" *> pure Unknown)

anyModelParser :: Parser Solution
anyModelParser = smt2ModelParser <|> defaultModelParser <|> return mempty

-- For the format CVC5 and Z3 use - what is it called?
defaultModelParser :: Parser Solution
defaultModelParser = do
  _       <- skipSpace >> char '(' >> skipSpace
  varSols <- many $ parseSomeSol <* skipSpace
  _       <- (skipSpace >> char ')' >> skipSpace) <|> skipSpace

  return $ fromSomeList varSols

smt2ModelParser :: Parser Solution
smt2ModelParser = do
  _       <- skipSpace >> char '(' >> skipSpace >> string "model" >> skipSpace
  varSols <- many $ parseSomeSol <* skipSpace
  _       <- (skipSpace >> char ')' >> skipSpace) <|> skipSpace

  return $ fromSomeList varSols

fromSomeList :: [SomeKnownSMTRepr SMTVarSol] -> Solution
fromSomeList = IM.fromList . fmap (\case someVarSol@(SomeKnownSMTRepr varSol) -> (coerce (smtVar varSol), someVarSol))

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
      parseRatioDouble <|> parseToRealDouble <|> rational
    BoolRepr -> string "Bool" >> skipSpace >> parseBool

parseBool :: Parser Bool
parseBool = (string "true" *> pure True) <|> (string "false" *> pure False)

parseRatioDouble :: Parser Double
parseRatioDouble = do
  _           <- char '(' >> skipSpace >> char '/' >> skipSpace
  numerator   <- decimal
  _           <- skipSpace
  denominator <- decimal
  _           <- skipSpace >> char ')'

  return $ fromRational $ numerator % denominator
  
parseToRealDouble :: Parser Double
parseToRealDouble = do
  _   <- char '(' >> skipSpace >> string "to_real" >> skipSpace
  dec <- decimal
  _   <- skipSpace >> char ')'
  
  return $ fromInteger dec
