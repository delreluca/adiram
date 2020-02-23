module Interpreter
    ( Command(Quit, Define, Evaluate)
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
                                                )
import           Data.Functor                   ( ($>) )

-- | A command issued to the interpreter
data Command = Quit -- ^ Quit interpreter
             | Define String (Expr String) -- ^ Define a free variable
             | Evaluate (Expr String) -- ^ Evaluate an expression

defParser :: Parsec String () Command
defParser = name >>= (\n -> Define n <$> parser)

-- | Parses commands. Commands either start with a colon or contain an expression to interprete.
commandParser :: Parsec String () Command
commandParser =
    (char ':' *> ((char 'q' $> Quit) <|> (string "def " *> defParser)))
        <|> (Evaluate <$> parser)
