module Interpreter
    ( Command(..)
    , EvaluationFlag(..)
    , commandParser
    , cbvRequested
    , normalOrderRequested
    , nameRequested
    )
where

import           ULC                            ( Expr )
import           ULC.Parser                     ( parser
                                                , name
                                                )
import           Text.Parsec                    ( Parsec
                                                , char
                                                , digit
                                                , string
                                                , (<|>)
                                                , many1
                                                , anyChar
                                                , optionMaybe
                                                )
import           Data.Functor                   ( ($>) )
import           Data.Maybe                     ( fromMaybe )
import           Control.Arrow                  ( (&&&) )

-- | A flag specified when requesting an evaluation
data EvaluationFlag = UseNormalOrder
                    | UseCallByValue
                    | DoNotEvaluate
                    | NameResult
                    deriving Eq

-- | A command issued to the interpreter
data Command = Quit -- ^ Quit interpreter
             | Load String -- ^ Load commands from file
             | Define String (Expr String) -- ^ Define a free variable
             | Evaluate [EvaluationFlag] (Expr String) -- ^ Evaluate an expression
             | SetIterationLimit !Int -- ^ Chane the iteration limit

evalRequested :: [EvaluationFlag] -> Bool
evalRequested = notElem DoNotEvaluate

cbvRequested :: [EvaluationFlag] -> Bool
cbvRequested = uncurry (&&) . (elem UseCallByValue &&& evalRequested)

normalOrderRequested :: [EvaluationFlag] -> Bool
normalOrderRequested = uncurry (&&) . (elem UseNormalOrder &&& evalRequested)

nameRequested :: [EvaluationFlag] -> Bool
nameRequested = uncurry (&&) . (elem NameResult &&& evalRequested)

flagsParser :: Parsec String () [EvaluationFlag]
flagsParser = char '!' *> many1 singleFlag <* char ' '
  where
    singleFlag =
        (char 'n' $> UseNormalOrder)
            <|> (char 'v' $> UseCallByValue)
            <|> (char 'p' $> DoNotEvaluate)
            <|> (char 'l' $> NameResult)

defParser :: Parsec String () Command
defParser = name >>= (\n -> Define n <$> parser)

loadParser :: Parsec String () Command
loadParser = Load <$> many1 anyChar

evalParser :: Parsec String () Command
evalParser = do
    flags <- optionMaybe flagsParser
    Evaluate (fromMaybe [UseNormalOrder] flags) <$> parser

settingParser :: Parsec String () Command
settingParser = SetIterationLimit . read <$> (string "maxiter " *> many1 digit)

-- | Parses commands. Commands either start with a colon or contain an expression to interprete.
commandParser :: Parsec String () Command
commandParser =
    (  char ':'
        *> (   (char 'q' $> Quit)
           <|> (string "def " *> defParser)
           <|> (string "load " *> loadParser)
           <|> (string "set " *> settingParser)
           )
        )
        <|> evalParser
