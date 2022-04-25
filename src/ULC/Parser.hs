module ULC.Parser
  ( name,
    parser,
    nat,
    PExpr,
  )
where

import Control.Monad.Fail (fail)
import Data.Text (cons, pack)
import Protolude hiding (list, many)
import Text.Parsec
  ( Parsec,
    alphaNum,
    chainl1,
    char,
    digit,
    eof,
    letter,
    many,
    many1,
    sepBy,
    spaces,
  )
import ULC
  ( Expr (App, Free),
    freeName,
    mkLam,
  )
import ULC.Church
  ( churchList,
    churchNumeral,
  )

type P a = Parsec Text () a

type PExpr = P (Expr Text)

name :: P Text
name = cons <$> letter <*> (pack <$> many alphaNum)

appl :: P (Expr Text -> Expr Text -> Expr Text)
appl = spaces $> App

nat :: P Int
nat =
  many1 digit
    >>= ( \cs -> case readEither cs of
            Left exn -> fail exn
            Right n -> pure n
        )

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
