module Helium.Parser (parseExpr) where

import Control.Arrow (left)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Void (Void)
import Helium.Syntax (Expr (..))
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

-- greedy space consumer
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

symbol :: String -> Parser String
symbol = L.symbol sc

keywords :: [String]
keywords = ["let", "in"]

keyword :: String -> Parser ()
keyword name = void . lexeme $ string name <* notFollowedBy alphaNumChar

identifier :: Parser String
identifier = lexeme $ do
  pos <- getOffset
  name <- (:) <$> letterChar <*> many alphaNumChar
  if name `elem` keywords
    then do
      setOffset pos
      fail $ "keyword " <> show name <> " cannot be used as an identifier"
    else pure name

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

prefix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ symbol name)

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [prefix "-" Neg],
    [binary "*" Mul, binary "/" Div],
    [binary "+" Add, binary "-" Sub]
  ]

atom :: Parser Expr
atom = parens expr <|> Lit <$> integer <|> Var <$> try identifier

term :: Parser Expr
term = foldl1 App <$> some atom

lambdaExpr :: Parser Expr
lambdaExpr = do
  _ <- symbol "\\"
  parameter <- identifier <?> "parameter"
  _ <- symbol "->"
  Lam parameter <$> expr

letExpr :: Parser Expr
letExpr = do
  keyword "let"
  name <- identifier <?> "identifier"
  _ <- symbol "="
  definition <- expr
  keyword "in"
  Let name definition <$> expr

expr :: Parser Expr
expr = letExpr <|> lambdaExpr <|> makeExprParser term operatorTable

parseExpr :: String -> Either String Expr
parseExpr input =
  left errorBundlePretty $
    parse (sc *> expr <* eof) "<stdin>" input
