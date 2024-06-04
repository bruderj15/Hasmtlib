{-# LANGUAGE LambdaCase #-}

module Language.Hasmtlib.Internal.Parser where

import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Orderable
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Boolean
import Language.Hasmtlib.Iteable
import Language.Hasmtlib.Codec
import Language.Hasmtlib.Type.Solution
import Data.Coerce
import Data.Ratio ((%))
import Data.ByteString
import Data.ByteString.Builder
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
  _     <- string $ toStrict $ toLazyByteString $ renderSMTLib2 (singRepr @t)
  _     <- skipSpace
  expr  <- parseExpr @t
  _     <- skipSpace >> char ')'

  -- Try to evaluate expression given by solver as solution
  -- Better: Take into scope already successfully parsed solutions for other vars.
  -- Is This even required though? Do the solvers ever answer like-wise?
  case decode mempty expr of
    Nothing    -> fail $ "Solver reponded with solution for var_" ++ show varId ++ " but it contains "
                      ++ "another var. This cannot be parsed and evaluated currently."
    Just value -> return $ SMTVarSol (coerce varId) (putValue value)
{-# INLINEABLE parseSol #-}

parseExpr :: forall t. KnownSMTRepr t => Parser (Expr t)
parseExpr = case singRepr @t of
  IntRepr  -> parseVar <|> parseConstant
          <|> parseUnOp "abs" abs <|> parseUnOp  "-" negate
          <|> parseBinOp "+" (+)  <|> parseBinOp "-" (-) <|> parseBinOp "+" (*) <|> parseBinOp "mod" Mod
          <|> parseToIntFun
          <|> parseIte
  RealRepr -> parseVar <|> parseConstant
          <|> parseUnOp "abs" abs <|> parseUnOp  "-" negate
          <|> parseBinOp "+" (+)  <|> parseBinOp "-" (-) <|> parseBinOp "+" (*) <|> parseBinOp "/" (/)
          <|> parseToRealFun
          <|> parseIte
          <|> parsePi <|> parseUnOp "sqrt" sqrt <|> parseUnOp "exp" exp <|> parseUnOp "log" log
          <|> parseUnOp "sin" sin <|> parseUnOp "cos" cos <|> parseUnOp "tan" tan
          <|> parseUnOp "arcsin" asin <|> parseUnOp "arccos" acos <|> parseUnOp "arctan" atan
--          <|> parseUnOp _ sinh <|> parseUnOp _ cosh <|> parseUnOp _ tanh
--          <|> parseUnOp _ asinh <|> parseUnOp _ acosh <|> parseUnOp _ atanh
  BoolRepr -> parseVar <|> parseConstant
          <|> parseBinOrdOp @t "<" (<?) <|> parseBinOrdOp @t "<=" (<=?)
          <|> parseBinOrdOp @t "=" (===)
          <|> parseBinOrdOp @t ">" (>?) <|> parseBinOrdOp @t ">=" (>=?)
          <|> parseIsIntFun
          <|> parseUnOp "not" not'
          <|> parseBinOp "and" (&&&)  <|> parseBinOp "or" (|||) <|> parseBinOp "=>" (==>) <|> parseBinOp "xor" xor

parseVar :: Parser (Expr t)
parseVar = do
  _     <- string "var_"
  varId <- decimal @Int

  return $ Var $ coerce varId
{-# INLINEABLE parseVar #-}

parseConstant :: forall t. KnownSMTRepr t => Parser (Expr t)
parseConstant = do
  cval <- case singRepr @t of
    IntRepr  -> anyValue decimal
    RealRepr -> anyValue parseRatioDouble <|> parseToRealDouble <|> anyValue rational
    BoolRepr -> parseBool

  return $ Constant $ putValue cval
{-# INLINEABLE parseConstant #-}

parseUnOp :: forall t. KnownSMTRepr t => ByteString -> (Expr t -> Expr t) -> Parser (Expr t)
parseUnOp opStr op = do
  _ <- char '(' >> skipSpace
  _ <- string opStr >> skipSpace
  val <- parseExpr
  _ <- skipSpace >> char ')'

  return $ op val
{-# INLINEABLE parseUnOp #-}

parseBinOp :: forall t. KnownSMTRepr t => ByteString -> (Expr t -> Expr t -> Expr t) -> Parser (Expr t)
parseBinOp opStr op = do
  _ <- char '(' >> skipSpace
  _ <- string opStr >> skipSpace
  l <- parseExpr
  _ <- skipSpace
  r <- parseExpr
  _ <- skipSpace >> char ')'

  return $ l `op` r
{-# INLINEABLE parseBinOp #-}

parseBinOrdOp :: forall t. KnownSMTRepr t => ByteString -> (Expr t -> Expr t -> Expr BoolType) -> Parser (Expr BoolType)
parseBinOrdOp opStr op = do
  _ <- char '(' >> skipSpace
  _ <- string opStr >> skipSpace
  l <- parseExpr
  _ <- skipSpace
  r <- parseExpr
  _ <- skipSpace >> char ')'

  return $ l `op` r
{-# INLINEABLE parseBinOrdOp #-}

parsePi :: Parser (Expr RealType)
parsePi = string "real.pi" *> return pi
{-# INLINEABLE parsePi #-}

parseToRealFun :: Parser (Expr RealType)
parseToRealFun = do
  _ <- char '(' >> skipSpace
  _ <- string "to_real" >> skipSpace
  val <- parseExpr
  _ <- skipSpace >> char ')'

  return $ ToReal val
{-# INLINEABLE parseToRealFun #-}

parseToIntFun :: Parser (Expr IntType)
parseToIntFun = do
  _ <- char '(' >> skipSpace
  _ <- string "to_int" >> skipSpace
  val <- parseExpr
  _ <- skipSpace >> char ')'

  return $ ToInt val
{-# INLINEABLE parseToIntFun #-}

parseIsIntFun :: Parser (Expr BoolType)
parseIsIntFun = do
  _ <- char '(' >> skipSpace
  _ <- string "is_int" >> skipSpace
  val <- parseExpr
  _ <- skipSpace >> char ')'

  return $ IsInt val
{-# INLINEABLE parseIsIntFun #-}

parseIte :: forall t. KnownSMTRepr t => Parser (Expr t)
parseIte = do
  _ <- char '(' >> skipSpace
  _ <- string "ite" >> skipSpace
  p <- parseExpr @BoolType
  _ <- skipSpace
  t <- parseExpr
  _ <- skipSpace
  f <- parseExpr
  _ <- skipSpace >> char ')'

  return $ ite p t f
{-# INLINEABLE parseIte #-}

anyValue :: Num a => Parser a -> Parser a
anyValue p = negativeValue p <|> p
{-# INLINEABLE anyValue #-}

negativeValue :: Num a => Parser a -> Parser a
negativeValue p = do
  _ <- char '(' >> skipSpace >> char '-' >> skipSpace
  val <- signed p
  _ <- skipSpace >> char ')'

  return $ negate val
{-# INLINEABLE negativeValue #-}

parseRatioDouble :: Parser Double
parseRatioDouble = do
  _           <- char '(' >> skipSpace >> char '/' >> skipSpace
  numerator   <- decimal
  _           <- skipSpace
  denominator <- decimal
  _           <- skipSpace >> char ')'

  return $ fromRational $ numerator % denominator
{-# INLINEABLE parseRatioDouble #-}

parseToRealDouble :: Parser Double
parseToRealDouble = do
  _   <- char '(' >> skipSpace >> string "to_real" >> skipSpace
  dec <- anyValue decimal
  _   <- skipSpace >> char ')'
  
  return $ fromInteger dec
{-# INLINEABLE parseToRealDouble #-}

parseBool :: Parser Bool
parseBool = (string "true" *> pure True) <|> (string "false" *> pure False)
{-# INLINEABLE parseBool #-}
