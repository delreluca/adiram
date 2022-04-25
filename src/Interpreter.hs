module Interpreter
    ( Command(..)
    , EvaluationFlag(..)
    , commandParser
    , cbvRequested
    , normalOrderRequested
    , nameRequested
    )
where

import Protolude
import Data.Text (pack)
import           ULC                            ( Expr )
import           ULC.Parser                     ( parser
                                                , name
                                                , nat
                                                )
import           Text.Parsec                    ( Parsec
                                                , char
                                                , string
                                                , many1
                                                , anyChar
                                                , optionMaybe
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Control.Arrow                  ( (&&&) )

-- | A flag specified when requesting an evaluation
data EvaluationFlag = UseNormalOrder
                    | UseCallByValue
                    | DoNotEvaluate
                    | NameResult
                    | PrintNumeral
                    deriving Eq

-- | A command issued to the interpreter
data Command = Quit -- ^ Quit interpreter
             | Load Text -- ^ Load commands from file
             | Define Text (Expr Text) -- ^ Define a free variable
             | Evaluate [EvaluationFlag] (Expr Text) -- ^ Evaluate an expression
             | SetIterationLimit !Int -- ^ Chane the iteration limit

evalRequested :: [EvaluationFlag] -> Bool
evalRequested = notElem DoNotEvaluate

cbvRequested :: [EvaluationFlag] -> Bool
cbvRequested = uncurry (&&) . (elem UseCallByValue &&& evalRequested)

normalOrderRequested :: [EvaluationFlag] -> Bool
normalOrderRequested = uncurry (&&) . (elem UseNormalOrder &&& evalRequested)

nameRequested :: [EvaluationFlag] -> Bool
nameRequested = uncurry (&&) . (elem NameResult &&& evalRequested)

flagsParser :: Parsec Text () [EvaluationFlag]
flagsParser = char '!' *> many1 singleFlag <* char ' '
  where
    singleFlag =
        (char 'n' $> UseNormalOrder)
            <|> (char 'v' $> UseCallByValue)
            <|> (char 'p' $> DoNotEvaluate)
            <|> (char 'l' $> NameResult)
            <|> (char 'z' $> PrintNumeral)

defParser :: Parsec Text () Command
defParser = name >>= (\n -> Define n <$> parser)

loadParser :: Parsec Text () Command
loadParser = Load . pack <$> many1 anyChar

evalParser :: Parsec Text () Command
evalParser = do
    flags <- optionMaybe flagsParser
    Evaluate (fromMaybe [UseNormalOrder] flags) <$> parser

settingParser :: Parsec Text () Command
settingParser = SetIterationLimit <$> (string "maxiter " *> nat)

-- | Parses commands. Commands either start with a colon or contain an expression to interprete.
commandParser :: Parsec Text () Command
commandParser =
    (  char ':'
        *> (   (char 'q' $> Quit)
           <|> (string "def " *> defParser)
           <|> (string "load " *> loadParser)
           <|> (string "set " *> settingParser)
           )
        )
        <|> evalParser
