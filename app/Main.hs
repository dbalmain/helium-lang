module Main (main) where

import Helium.Eval (eval)
import Helium.Parser (parseExpr)
import System.Console.Haskeline

main :: IO ()
main = do
  putStrLn "Helium Chapter 2 - Type an expression (Ctrl-D to quit)"
  runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  minput <- getInputLine "h> "
  case minput of
    Nothing -> outputStrLn ""
    Just input -> do
      case parseExpr input of
        Left err -> outputStrLn $ "Parse Error: " <> err
        Right expr -> do
          outputStrLn $ show expr
          case eval expr of
            Left err -> outputStrLn $ "Runtime Error: " <> err
            Right result -> outputStrLn $ show result
      repl
