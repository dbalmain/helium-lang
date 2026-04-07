module Helium.Parser (parseExpr) where

import Control.Arrow (left)
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (InfixL, Prefix), makeExprParser)
import Data.Void (Void)
import Helium.Syntax (Expr (..), Span (Span), Type (..), spanOf)
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

spanned :: Parser a -> Parser (Span, a)
spanned p = do
  start <- getSourcePos
  result <- p
  end <- getSourcePos
  pure (Span start end, result)

prefix :: String -> (Span -> Expr -> Expr) -> Operator Parser Expr
prefix name f = Prefix $ do
  start <- getSourcePos
  _ <- symbol name
  pure $ \a -> f (Span start (spanEnd (spanOf a))) a
  where
    spanEnd (Span _ e) = e

binary :: String -> (Span -> Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL $ do
  _ <- symbol name
  pure $ \a b ->
    let Span start _ = spanOf a
        Span _ end = spanOf b
     in f (Span start end) a b

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [prefix "-" Neg],
    [binary "*" Mul, binary "/" Div],
    [binary "+" Add, binary "-" Sub]
  ]

-- Type parsing

typeAtom :: Parser Type
typeAtom = TInt <$ keyword "Int" <|> parens typeExpr

typeExpr :: Parser Type
typeExpr = foldr1 TFun <$> sepBy1 typeAtom (symbol "->")

-- Expression parsing

atom :: Parser Expr
atom =
  parens expr
    <|> (uncurry Lit <$> spanned integer)
    <|> (uncurry Var <$> spanned (try identifier))

term :: Parser Expr
term = foldl1 mkApp <$> some atom
  where
    mkApp f a =
      let Span start _ = spanOf f
          Span _ end = spanOf a
       in App (Span start end) f a

withSpan :: (Span -> a -> b -> c -> Expr) -> Parser (a, b, c) -> Parser Expr
withSpan ex p = do
  (span_, (x, y, z)) <- spanned p
  pure $ ex span_ x y z

lambdaExpr :: Parser Expr
lambdaExpr = withSpan Lam $ do
  _ <- symbol "\\"
  _ <- symbol "("
  parameter <- identifier <?> "parameter"
  _ <- symbol ":"
  ty <- typeExpr
  _ <- symbol ")"
  _ <- symbol "->"
  body <- expr
  pure (parameter, ty, body)

letExpr :: Parser Expr
letExpr = withSpan Let $ do
  keyword "let"
  name <- identifier <?> "identifier"
  _ <- symbol "="
  definition <- expr
  keyword "in"
  body <- expr
  pure (name, definition, body)

expr :: Parser Expr
expr = letExpr <|> lambdaExpr <|> makeExprParser term operatorTable

parseExpr :: String -> Either String Expr
parseExpr input =
  left errorBundlePretty $
    parse (sc *> expr <* eof) "<stdin>" input
