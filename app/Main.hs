module Main where

import ULC
import System.IO (hFlush, stdout)
import qualified Text.Parsec as P

readUserInput :: IO (Maybe String)
readUserInput = do
  putStr "ULC> "
  hFlush stdout
  input <- getLine
  return $ case input of
    ":q" -> Nothing
    _ -> Just input

tellBack :: String -> IO ()
tellBack s = putStrLn $ either (const "Error parsing. Use :q to quit.") showExpr (P.parse (region :: P.Parsec String () (Expr ApoString)) "$repl" s)

repl :: IO ()
repl = readUserInput >>= maybe (putStrLn "Goodbye") (\s -> tellBack s >> repl)

main :: IO ()
main = putStrLn "Welcome to adiram" >> repl
