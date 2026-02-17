module Helium.Parser (parseExpr) where

import Control.Arrow (left)
import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Void (Void)
import Helium.Syntax (Expr (..))
import Text.Megaparsec
import Text.Megaparsec.Char (char, space1, string)
import Text.Megaparsec.Char.Lexer qualified as L

type Parser = Parsec Void String

-- greedy space consumer
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Int
integer = lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (lexeme (char '(')) (lexeme (char ')'))

prefix :: String -> (Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix (f <$ lexeme (string name))

binary :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ lexeme (string name))

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [prefix "-" Neg],
    [binary "*" Mul, binary "/" Div],
    [binary "+" Add, binary "-" Sub]
  ]

term :: Parser Expr
term = parens expr <|> Lit <$> integer

expr :: Parser Expr
expr = makeExprParser term operatorTable

parseExpr :: String -> Either String Expr
parseExpr input =
  left errorBundlePretty $
    parse (sc *> expr <* eof) "<stdin>" input
