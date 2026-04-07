module Main (main) where

import Data.List (intercalate, (!?))
import Data.Maybe (fromMaybe)
import Helium.Check (elaborate)
import Helium.Eval (eval)
import Helium.Parser (parseExpr)
import Helium.Syntax (TypeError (TypeError), spanStart)
import System.Console.Isocline

process :: String -> IO ()
process input =
  case parseExpr input of
    Left err -> putStrLn $ "Parse Error: " <> err
    Right expr -> do
      case elaborate expr of
        Left err -> putStrLn $ renderTypeError input err
        Right (ty, core) -> do
          putStrLn $ "Type: " <> show ty
          case eval mempty core of
            Left err -> putStrLn $ "Runtime Error: " <> err
            Right result -> print result

repl :: IO ()
repl = do
  minput <- readlineMaybe "he"
  case minput of
    Nothing -> putStrLn ""
    Just input -> process input >> repl

main :: IO ()
main = do
  putStrLn "Helium Chapter 5 - Type an expression (Ctrl-D to quit, Ctrl-Enter for newline)"
  setHistory "helium_history.txt" 200
  _ <- enableMultiline True
  repl

renderTypeError :: String -> TypeError -> String
renderTypeError source (TypeError span_ msg) =
  let (ln, col) = spanStart span_
      sourceLines = lines source
      lineText = fromMaybe "" (sourceLines !? (ln - 1))
      pointer = replicate (col - 1) ' ' <> "^"
      header = "Type Error at " <> show ln <> ":" <> show col <> ":"
      lineNumber = show ln
      divider = " | "
   in intercalate
        "\n   "
        [ header,
          lineNumber <> divider <> lineText,
          replicate (length lineNumber) ' ' <> divider <> pointer,
          msg
        ]
