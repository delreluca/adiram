module ULC.Parser
    ( name
    , parser
    , nat
    , PExpr
    )
where

import Protolude hiding (many,list)
import Data.Text (pack, cons)
import Control.Monad.Fail (fail)
import           ULC                            ( Expr(App, Free)
                                                , freeName
                                                , mkLam
                                                )
import           ULC.Church                     ( churchList
                                                , churchNumeral
                                                )
import           Text.Parsec                    ( eof
                                                , spaces
                                                , char
                                                , letter
                                                , alphaNum
                                                , digit
                                                , many
                                                , many1
                                                , chainl1
                                                , sepBy
                                                , Parsec
                                                )
type P a = Parsec Text () a
type PExpr = P (Expr Text)

name :: P Text
name = cons <$> letter <*> (pack <$> many alphaNum)

appl :: P (Expr Text -> Expr Text -> Expr Text)
appl = spaces $> App

nat :: P Int
nat = many1 digit >>= (\cs -> case readEither cs of 
                                Left exn -> fail exn
                                Right n -> pure n) 

list :: P [Expr Text]
list = char '[' *> sepBy token (char ',') <* char ']'

free, cList, cNat, abst, paren, token, term :: PExpr
free = Free <$> name
cNat = churchNumeral "s" "z" <$> nat
cList = churchList "c" "n" (\n -> headDef "??" . freeName n) <$> list
abst = mkLam <$> (char '\\' *> name <* char '.') <*> term
paren = char '(' *> term <* char ')'
token = spaces *> (free <|> cNat <|> abst <|> cList <|> paren) <* spaces
term = chainl1 token appl

-- | Parses a full (i.e. awaiting EOF at the end) expression in the untyped lambda calculus
parser :: PExpr
parser = term <* eof
