{-# LANGUAGE LambdaCase #-}

module Language.Hasmtlib.Internal.Parser where

import Prelude hiding (readFile)  
  
import Language.Hasmtlib.Internal.Bitvec
import Language.Hasmtlib.Internal.Render
import Language.Hasmtlib.Internal.Expr
import Language.Hasmtlib.Equatable
import Language.Hasmtlib.Orderable
import Language.Hasmtlib.Boolean
import Language.Hasmtlib.Iteable
import Language.Hasmtlib.Codec
import Language.Hasmtlib.Type.Solution
import Data.Bit
import Data.Coerce
import Data.Proxy
import Data.Ratio ((%))
import Data.ByteString
import Data.ByteString.Builder
import Data.Attoparsec.ByteString hiding (Result, skipWhile)
import Data.Attoparsec.ByteString.Char8 hiding (Result)
import qualified Data.IntMap as IM
import Control.Applicative
import GHC.TypeNats

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
           <|> parseAnyBvUpToLength 128

parseAnyBvUpToLength :: Natural -> Parser (SomeKnownSMTRepr SMTVarSol)
parseAnyBvUpToLength hi = asum $ fmap ((\case SomeNat p -> goProxy p) . someNatVal) [0..hi]
  where
    goProxy :: forall n. KnownNat n => Proxy n -> Parser (SomeKnownSMTRepr SMTVarSol)
    goProxy _ = SomeKnownSMTRepr <$> parseSol @(BvType n)

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
parseExpr = var <|> constant <|> smtIte
        <|> case singRepr @t of
              IntRepr  -> unary "abs" abs <|> unary  "-" negate
                      <|> binary "+" (+)  <|> binary "-" (-) <|> binary "*" (*) <|> binary "mod" Mod
                      <|> toIntFun
              RealRepr -> unary "abs" abs <|> unary  "-" negate
                      <|> binary "+" (+)  <|> binary "-" (-) <|> binary "*" (*) <|> binary "/" (/)
                      <|> toRealFun
                      <|> smtPi <|> unary "sqrt" sqrt <|> unary "exp" exp
                      <|> unary "sin" sin <|> unary "cos" cos <|> unary "tan" tan
                      <|> unary "arcsin" asin <|> unary "arccos" acos <|> unary "arctan" atan
              BoolRepr -> parseIsIntFun
                      <|> unary "not" not'
                      <|> binary "and" (&&&)  <|> binary "or" (|||) <|> binary "=>" (==>) <|> binary "xor" xor
                      <|> binary @IntType  "=" (===) <|> binary @IntType  "distinct" (/==)
                      <|> binary @RealType "=" (===) <|> binary @RealType "distinct" (/==)
                      <|> binary @RealType "=" (===) <|> binary @RealType "distinct" (/==)
                      <|> binary @IntType "<" (<?) <|> binary @IntType "<=" (<=?)
                      <|> binary @IntType ">=" (>=?) <|> binary @IntType ">" (>?)
                      <|> binary @RealType "<" (<?) <|> binary @RealType "<=" (<=?)
                      <|> binary @RealType ">=" (>=?) <|> binary @RealType ">" (>?)
                      -- TODO: All (?) bv lengths - also for '=' and 'distinct'
--                      <|> binary @(BvType 10) "bvult" (<?) <|> binary @(BvType 10) "bvule" (<=?)
--                      <|> binary @(BvType 10) "bvuge" (>=?) <|> binary @(BvType 10) "bvugt" (>?)
              BvRepr _ -> unary "bvnot" not'
                      <|> binary "bvand" (&&&)  <|> binary "bvor" (|||) <|> binary "bvxor" xor <|> binary "bvnand" BvNand <|> binary "bvnor" BvNor
                      <|> unary  "bvneg" negate
                      <|> binary "bvadd" (+)  <|> binary "bvsub" (-) <|> binary "bvmul" (*)
                      <|> binary "bvudiv" BvuDiv <|> binary "bvurem" BvuRem
                      <|> binary "bvshl" BvShL <|> binary "bvlshr" BvLShR

var :: Parser (Expr t)
var = do
  _     <- string "var_"
  varId <- decimal @Int

  return $ Var $ coerce varId
{-# INLINEABLE var #-}

constant :: forall t. KnownSMTRepr t => Parser (Expr t)
constant = do
  cval <- case singRepr @t of
    IntRepr  -> anyValue decimal
    RealRepr -> anyValue parseRatioDouble <|> parseToRealDouble <|> anyValue rational
    BoolRepr -> parseBool
    BvRepr p -> anyBitvector p

  return $ Constant $ putValue cval
{-# INLINEABLE constant #-}

anyBitvector :: KnownNat n => Proxy n -> Parser (Bitvec n)
anyBitvector p = binBitvector p <|> hexBitvector p <|> literalBitvector p

binBitvector :: KnownNat n => Proxy n -> Parser (Bitvec n)
binBitvector p = do
  _  <- string "#b" >> skipSpace
  bs <- many $ char '0' <|> char '1'
  let bs' :: [Bit] = fmap (\b -> ite (b == '1') true false) bs
  case bvFromListN' p bs' of
    Nothing -> fail $ "Expected BitVector of length" <> show (natVal p) <> ", but got a different one"
    Just v  -> return v
{-# INLINEABLE binBitvector #-}

hexBitvector :: KnownNat n => Proxy n -> Parser (Bitvec n)
hexBitvector _ = do
  _ <- string "#x" >> skipSpace
  fromInteger <$> hexadecimal
{-# INLINEABLE hexBitvector #-}

literalBitvector :: KnownNat n => Proxy n -> Parser (Bitvec n)
literalBitvector _ = do
  _ <- char '(' >> skipSpace
  _ <- char '_' >> skipSpace
  _ <- string "bv"
  x <- decimal
  _ <- skipWhile (/= ')') >> char ')'

  return $ fromInteger x
{-# INLINEABLE literalBitvector #-}

unary :: forall t r. KnownSMTRepr t => ByteString -> (Expr t -> Expr r) -> Parser (Expr r)
unary opStr op = do
  _ <- char '(' >> skipSpace
  _ <- string opStr >> skipSpace
  val <- parseExpr
  _ <- skipSpace >> char ')'

  return $ op val
{-# INLINEABLE unary #-}

binary :: forall t r. KnownSMTRepr t => ByteString -> (Expr t -> Expr t -> Expr r) -> Parser (Expr r)
binary opStr op = do
  _ <- char '(' >> skipSpace
  _ <- string opStr >> skipSpace
  l <- parseExpr
  _ <- skipSpace
  r <- parseExpr
  _ <- skipSpace >> char ')'

  return $ l `op` r
{-# INLINEABLE binary #-}

smtPi :: Parser (Expr RealType)
smtPi = string "real.pi" *> return pi
{-# INLINEABLE smtPi #-}

toRealFun :: Parser (Expr RealType)
toRealFun = do
  _ <- char '(' >> skipSpace
  _ <- string "to_real" >> skipSpace
  val <- parseExpr
  _ <- skipSpace >> char ')'

  return $ ToReal val
{-# INLINEABLE toRealFun #-}

toIntFun :: Parser (Expr IntType)
toIntFun = do
  _ <- char '(' >> skipSpace
  _ <- string "to_int" >> skipSpace
  val <- parseExpr
  _ <- skipSpace >> char ')'

  return $ ToInt val
{-# INLINEABLE toIntFun #-}

parseIsIntFun :: Parser (Expr BoolType)
parseIsIntFun = do
  _ <- char '(' >> skipSpace
  _ <- string "is_int" >> skipSpace
  val <- parseExpr
  _ <- skipSpace >> char ')'

  return $ IsInt val
{-# INLINEABLE parseIsIntFun #-}

smtIte :: forall t. KnownSMTRepr t => Parser (Expr t)
smtIte = do
  _ <- char '(' >> skipSpace
  _ <- string "ite" >> skipSpace
  p <- parseExpr @BoolType
  _ <- skipSpace
  t <- parseExpr
  _ <- skipSpace
  f <- parseExpr
  _ <- skipSpace >> char ')'

  return $ ite p t f
{-# INLINEABLE smtIte #-}

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
