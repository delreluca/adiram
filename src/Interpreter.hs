module Interpreter
    ( Command(Quit, Define, Load, Evaluate)
    , commandParser
    )
where

import           ULC                            ( Expr )
import           ULC.Parser                     ( parser
                                                , name
                                                )
import           Text.Parsec                    ( Parsec
                                                , char
                                                , string
                                                , (<|>)
                                                , many1
                                                , anyChar
                                                )
import           Data.Functor                   ( ($>) )

-- | A command issued to the interpreter
data Command = Quit -- ^ Quit interpreter
             | Load String -- ^ Load commands from file
             | Define String (Expr String) -- ^ Define a free variable
             | Evaluate (Expr String) -- ^ Evaluate an expression

defParser :: Parsec String () Command
defParser = name >>= (\n -> Define n <$> parser)

loadParser :: Parsec String () Command
loadParser = Load <$> many1 anyChar

-- | Parses commands. Commands either start with a colon or contain an expression to interprete.
commandParser :: Parsec String () Command
commandParser =
    (  char ':'
        *> (   (char 'q' $> Quit)
           <|> (string "def " *> defParser)
           <|> (string "load " *> loadParser)
           )
        )
        <|> (Evaluate <$> parser)
