module Main (main) where

import Helium.Parser (parseExpr)
import System.Console.Haskeline

main :: IO ()
main = do
  putStrLn "Helium Chapter 1 - Type an expression (Ctrl-D to quit)"
  runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  minput <- getInputLine "h> "
  case minput of
    Nothing -> outputStrLn ""
    Just input -> do
      case parseExpr input of
        Right expr -> outputStrLn $ show expr
        Left err -> outputStrLn $ "Error: " <> err
      repl
