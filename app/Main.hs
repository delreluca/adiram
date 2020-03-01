module Main where

import           ULC
import           Interpreter                    ( Command(..)
                                                , EvaluationFlag(..)
                                                , commandParser
                                                , cbvRequested
                                                , normalOrderRequested
                                                )
import           Control.Monad                  ( void
                                                , foldM
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( ($>) )
import           Data.Map                       ( empty
                                                , insert
                                                )
import qualified System.Console.Haskeline      as H
import           System.IO                      ( hClose
                                                , hGetContents
                                                , openFile
                                                , IOMode(ReadMode)
                                                )
import           Text.Parsec                    ( parse )


repl :: Baton -> H.InputT IO Baton
repl b = do
        input <- H.getInputLine "λ "
        b'    <- maybe (pure Stop) (execCommand "$repl" b) input
        case b' of
                Stop -> H.outputStrLn "Goodbye" $> b
                _    -> repl b'

data Baton = Stop | Cont (Environment String)

execCommand :: String -> Baton -> String -> H.InputT IO Baton
execCommand _ Stop     _   = pure Stop
execCommand f (Cont g) cmd = case parse commandParser f cmd of
        Left x ->
                H.outputStrLn ("Error parsing. Use :q to quit.\n\n" ++ show x)
                        $> Cont g
        Right Quit -> pure Stop
        Right (Define n e) ->
                pure $ Cont (Environment $ insert n e (freeVars g))
        Right (Load f'      ) -> loadFile (Cont g) f'
        Right (Evaluate fs e) -> liftIO (tellBack g fs e) $> Cont g

tellBack :: Environment String -> [EvaluationFlag] -> Expr String -> IO ()
tellBack g fs e = putStrLn $ prettyPrint e ++ (cbv fs) ++ (no fs)
    where
        cbv fs' | cbvRequested fs' =
                "\nCall-by-value yields: " ++ prettyPrint (reduceCbv g e)
        cbv _ = ""
        no fs' | normalOrderRequested fs' =
                "\nNormal order yields:  " ++ prettyPrint (reduceNormal g e)
        no _ = ""

-- TODO: consider using Text or ByteString for strict IO.

strictify :: IO String -> IO String
strictify m = m >>= (\s -> length s `seq` return s)

fileLines :: String -> IO [String]
fileLines path = do
        handle <- openFile path ReadMode
        s      <- lines <$> strictify (hGetContents handle)
        hClose handle
        pure s

loadFile :: Baton -> String -> H.InputT IO Baton
loadFile g path = liftIO (fileLines path) >>= foldM (execCommand path) g

main :: IO ()
main = putStrLn "Welcome to adiram"
        >> void (H.runInputT H.defaultSettings $ repl startEnv)
        where startEnv = Cont $ Environment empty
