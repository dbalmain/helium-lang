module Main (main) where

import Helium.Check (elaborate)
import Helium.Eval (eval)
import Helium.Parser (parseExpr)
import System.Console.Isocline

process :: String -> IO ()
process input =
  case parseExpr input of
    Left err -> putStrLn $ "Parse Error: " <> err
    Right expr -> do
      print expr
      case elaborate expr of
        Left err -> putStrLn $ "Type Error: " <> err
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
