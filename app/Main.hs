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
tellBack s = putStrLn $ either badCase niceCase (P.parse parser "$repl" s)
    where niceCase e = prettyPrint e ++ "\nCall-by-value yields: " ++ prettyPrint (reduceCbv e) ++ "\nNormal order yields:  " ++ prettyPrint (reduceNormal e)
          badCase x = "Error parsing. Use :q to quit.\n\n" ++ show x

repl :: IO ()
repl = readUserInput >>= maybe (putStrLn "Goodbye") (\s -> tellBack s >> repl)

main :: IO ()
main = putStrLn "Welcome to adiram" >> repl
