module Main where

import           ULC
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import qualified Text.Parsec                   as P
import qualified System.Console.Haskeline      as H
import           Control.Monad.IO.Class         ( liftIO )

repl :: H.InputT IO ()
repl = do
  input <- H.getInputLine "λ "
  case input of
    Nothing   -> return ()
    Just ":q" -> H.outputStrLn "Goodbye"
    Just x    -> liftIO (tellBack x) >> repl

tellBack :: String -> IO ()
tellBack s = putStrLn $ either badCase niceCase (P.parse parser "$repl" s)
 where
  niceCase e =
    prettyPrint e
      ++ "\nCall-by-value yields: "
      ++ prettyPrint (reduceCbv e)
      ++ "\nNormal order yields:  "
      ++ prettyPrint (reduceNormal e)
  badCase x = "Error parsing. Use :q to quit.\n\n" ++ show x
main :: IO ()
main = putStrLn "Welcome to adiram" >> H.runInputT H.defaultSettings repl
