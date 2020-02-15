module Main where

import           ULC
import           Interpreter                   as I
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import qualified Text.Parsec                   as P
import qualified System.Console.Haskeline      as H
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Map                       ( empty
                                                , insert
                                                )


repl :: Environment String -> H.InputT IO (Environment String)
repl g = do
  input <- H.getInputLine "λ "
  case P.parse I.commandParser "" <$> input of
    Nothing -> return g
    Just (Left x) ->
      H.outputStrLn ("Error parsing. Use :q to quit.\n\n" ++ show x) >> repl g
    Just (Right I.Quit        ) -> H.outputStrLn "Goodbye" >> return g
    Just (Right (I.Define n e)) -> repl (Environment $ insert n e (freeVars g))
    Just (Right (I.Evaluate e)) -> liftIO (tellBack g e) >> repl g

tellBack :: Environment String -> Expr String -> IO ()
tellBack g e =
  putStrLn
    $  prettyPrint e
    ++ "\nCall-by-value yields: "
    ++ prettyPrint (reduceCbv g e)
    ++ "\nNormal order yields:  "
    ++ prettyPrint (reduceNormal g e)

main :: IO ()
main = putStrLn "Welcome to adiram"
  >> void (H.runInputT H.defaultSettings $ repl $ Environment empty)
