module ULC.Parser
    ( name
    , parser
    , PExpr
    )
where

import           ULC                            ( Expr(App, Free)
                                                , mkLam
                                                )
import           ULC.Church                     ( churchNumeral )
import           Data.Functor                   ( ($>) )
import           Text.Parsec                    ( (<|>)
                                                , eof
                                                , spaces
                                                , char
                                                , letter
                                                , alphaNum
                                                , digit
                                                , many
                                                , many1
                                                , chainl1
                                                , Parsec
                                                )

type P a = Parsec String () a
type PExpr = P (Expr String)

name :: P String
name = (:) <$> letter <*> many alphaNum

appl :: P (Expr String -> Expr String -> Expr String)
appl = spaces $> App

nat :: P Int
nat = read <$> many1 digit

free, cNat, abst, paren, token, term :: PExpr
free = Free <$> name
cNat = churchNumeral "s" "z" <$> nat
abst = mkLam <$> (char '\\' *> name <* char '.') <*> term
paren = char '(' *> term <* char ')'
token = spaces *> (free <|> cNat <|> abst <|> paren) <* spaces
term = chainl1 token appl

-- | Parses a full (i.e. awaiting EOF at the end) expression in the untyped lambda calculus
parser :: PExpr
parser = term <* eof
