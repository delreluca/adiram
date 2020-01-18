module ULC2.Parsing
    ( term0
    )
where

import           Data.Functor                   ( ($>) )
import qualified Text.Parsec                   as P
import           Text.Parsec                    ( (<|>)
                                                , (<?>)
                                                )
import qualified ULC                           as L

name :: P.Parsec String () String
name = (:) <$> P.letter <*> P.many P.alphaNum

free :: P.Parsec String () (L.Expr String)
free = L.Free <$> name

abst :: P.Parsec String () (L.Expr String)
abst = L.mkLam <$> (P.char '\\' *> name <* P.char '.') <*> term

appl :: P.Parsec String () (L.Expr String -> L.Expr String -> L.Expr String)
appl = P.spaces $> L.App

paren :: P.Parsec String () (L.Expr String)
paren = P.char '(' *> term <* P.char ')'

token :: P.Parsec String () (L.Expr String)
token = P.spaces *> (free <|> abst <|> paren) <* P.spaces

term :: P.Parsec String () (L.Expr String)
term = P.chainl1 token appl

term0 :: P.Parsec String () (L.Expr String)
term0 = term <* P.eof
