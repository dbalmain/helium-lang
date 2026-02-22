module Main (main) where

import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Helium.Eval (eval)
import Helium.Parser (parseExpr)
import System.Console.Haskeline

main :: IO ()
main = do
  putStrLn "Helium Chapter 2 - Type an expression (Ctrl-D to quit)"
  runInputT defaultSettings repl

liftWithPrefix :: (MonadError String m) => String -> Either String a -> m a
liftWithPrefix prefix (Left err) = throwError (prefix <> err)
liftWithPrefix _ (Right val) = pure val

run :: (MonadError String m, MonadIO m) => String -> m Int
run input = do
  expr <- liftWithPrefix "Parser Error: " (parseExpr input)
  liftIO $ print expr
  liftWithPrefix "Runtime Error: " (eval mempty expr)

repl :: InputT IO ()
repl = do
  minput <- getInputLine "h> "
  case minput of
    Nothing -> outputStrLn ""
    Just input -> do
      outcome <- runExceptT (run input)
      either outputStrLn (outputStrLn . show) outcome

      repl
