module Main (main) where

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
      outputStrLn input
      repl
