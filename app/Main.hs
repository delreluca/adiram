module Main where

import           Protolude               hiding ( empty )
import           ULC
import           Data.Text                      ( lines
                                                , pack
                                                , unpack
                                                )
import           Data.Text.IO                   ( readFile )
import           Data.Text.Lazy                 ( toStrict )
import           Data.Text.Lazy.Builder         ( toLazyText )
import           Data.Text.Lazy.Builder.Int     ( decimal )
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
import           Data.Map                       ( empty
                                                , keys
                                                , insert
                                                )
import qualified Data.Map                      as M
                                                ( filter )
import qualified System.Console.Haskeline      as H
import           Text.Parsec                    ( parse )


repl :: Baton -> H.InputT IO Baton
repl b = do
        input <- pack <<$>> H.getInputLine "λ "
        b'    <- maybe (pure Stop) (execCommand "$repl" b) input
        case b' of
                Stop -> putText "Goodbye" $> b
                _    -> repl b'

data Baton = Stop | Cont (Environment Text)

execCommand :: Text -> Baton -> Text -> H.InputT IO Baton
execCommand _ Stop     _   = pure Stop
execCommand f (Cont g) cmd = case parse commandParser (unpack f) cmd of
        Left x -> putText ("Error parsing. Use :q to quit.\n\n" <> show x)
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
                putText
                                (  "Iteration limit is now "
                                <> toStrict (toLazyText $ decimal lim)
                                )
                        $> Cont (Environment (freeVars g) lim)

findName :: Environment Text -> Expr Text -> Maybe Text
findName g x = firstOrNot $ keys $ M.filter (x ~=) (freeVars g)
    where
        (~=) a b = normalise a == normalise b
        normalise a = stripNames "_" (reduceNormal g a)
        firstOrNot (a : _) = Just a
        firstOrNot _       = Nothing

tellBack :: Environment Text -> [EvaluationFlag] -> Expr Text -> IO ()
tellBack g fs e = putText $ prettyPrint e <> cbv fs <> no fs <> nam fs
    where
        cbv fs' | cbvRequested fs' =
                "\nCall-by-value yields: " <> prettyPrint (reduceCbv g e)
        cbv _ = ""
        no fs' | normalOrderRequested fs' =
                "\nNormal order yields:  " <> prettyPrint (reduceNormal g e)
        no _ = ""
        nam fs' | nameRequested fs' = maybe
                "\nThis does not match any known variable"
                ("\nThis is equivalent to " <>)
                (findName g e)
        nam _ = ""

fileLines :: Text -> IO [Text]
fileLines path = lines <$> readFile (unpack path)

loadFile :: Baton -> Text -> H.InputT IO Baton
loadFile g path = liftIO (fileLines path) >>= foldM (execCommand path) g

main :: IO ()
main = putText "Welcome to adiram"
        >> void (H.runInputT H.defaultSettings $ repl startEnv)
        where startEnv = Cont $ Environment empty 10000
