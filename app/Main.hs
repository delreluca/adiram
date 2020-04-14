module Main where

import           ULC
import           Interpreter                    ( Command(..)
                                                , EvaluationFlag(..)
                                                , commandParser
                                                , cbvRequested
                                                , normalOrderRequested
                                                , nameRequested
                                                )
import           Control.Monad                  ( void
                                                , foldM
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Data.Functor                   ( ($>) )
import           Data.Map                       ( empty
                                                , keys
                                                , insert
                                                )
import qualified Data.Map                      as M
                                                ( filter )
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
                pure
                        $ Cont
                                  (Environment (insert n e (freeVars g))
                                               (iterationLimit g)
                                  )
        Right (Load f'      ) -> loadFile (Cont g) f'
        Right (Evaluate fs e) -> liftIO (tellBack g fs e) $> Cont g
        Right (SetIterationLimit lim) ->
                H.outputStrLn ("Iteration limit is now " ++ show lim)
                        $> Cont (Environment (freeVars g) lim)

findName :: Environment String -> Expr String -> Maybe String
findName g x = firstOrNot $ keys $ M.filter (x ~=) (freeVars g)
    where
        (~=) a b = normalise a == normalise b
        normalise a = stripNames "_" (reduceNormal g a)
        firstOrNot (a : _) = Just a
        firstOrNot _       = Nothing

tellBack :: Environment String -> [EvaluationFlag] -> Expr String -> IO ()
tellBack g fs e = putStrLn $ prettyPrint e ++ cbv fs ++ no fs ++ nam fs
    where
        cbv fs' | cbvRequested fs' =
                "\nCall-by-value yields: " ++ prettyPrint (reduceCbv g e)
        cbv _ = ""
        no fs' | normalOrderRequested fs' =
                "\nNormal order yields:  " ++ prettyPrint (reduceNormal g e)
        no _ = ""
        nam fs' | nameRequested fs' = maybe
                "\nThis does not match any known variable"
                ("\nThis is equivalent to " ++)
                (findName g e)
        nam _ = ""

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
        where startEnv = Cont $ Environment empty 10000
