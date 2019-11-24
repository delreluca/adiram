{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ULC
    ( ExprScope()
    , Expr(Free, App, Lam)
    , ApoString(MkApoString)
    , VarName
    , mkLam
    , showExpr
    , region
    , reduce
    )
where

import           Data.String
import qualified Text.Parsec                   as P
import           Text.Parsec                    ( (<|>)
                                                , between
                                                , char
                                                , many
                                                , alphaNum
                                                , letter
                                                , spaces
                                                , sepBy
                                                )

-- | A scoped expression that contains bound variables.
newtype ExprScope a = ExprScope (Expr a)
    deriving (Eq, Show)

-- | Represents an expression in the untyped lambda calculucs, where de Brujin indices are used for bound variables.
-- Note that 'Bound' is not exported from this module, use 'mkLam' to create lambda abstractions binding variables.
data Expr a = Free a -- ^ A free variable
            | Bound !Int -- ^ A bound variable containing its de Brujin index
            | App (Expr a) (Expr a) -- ^ An application of a right to a left expression
            | Lam a (ExprScope a) -- ^ An abstraction with a scoped inner term with bound variables and a preferred name for it
    deriving (Eq, Show)

-- | Takes an expression and turns it into a scope by binding a given free variable.
abstract :: Eq a => a -> Expr a -> ExprScope a
abstract nameToBind e = ExprScope $ go 0 e
  where
    go level (Bound i) = Bound i
    go level (Free a) | nameToBind == a = Bound level
                      | otherwise       = Free a
    go level (App l r) = go level l `App` go level r
    go level (Lam a (ExprScope inner)) =
        Lam a $ ExprScope $ go (succ level) inner

-- | Returns a lambda abstraction by binding a given free variable and using it as preferred name
mkLam :: Eq a => a -> Expr a -> Expr a
mkLam a = Lam a . abstract a

class (IsString a, Eq a) => VarName a where
    varToString :: a -> String
    freeName :: a -> [a] -> [a]

-- | A variable name that uses arbitrary strings and adds apostrophes on name collisions
newtype ApoString = MkApoString { apoToString :: String } deriving (Eq, Show)

-- | Defines conversions to strings and free names for variable names, for use in printing expressions
instance VarName ApoString where
    varToString = apoToString
    freeName    = freeNameWithApo

instance IsString ApoString where
    fromString = MkApoString

-- | Finds a free name for a variable by appending apostrophes if necessary
freeNameWithApo :: ApoString -> [ApoString] -> [ApoString]
freeNameWithApo n ns
    | n `notElem` ns = n : ns
    | otherwise      = freeNameWithApo (MkApoString (apoToString n ++ "\'")) ns

-- | Prints a lambda expression; not pretty
showExpr :: VarName a => Expr a -> String
showExpr = go []
  where
    go _  (Free  a) = varToString a
    go ns (Bound i) = varToString $ ns !! i
    go ns (App l r) = "(" ++ go ns l ++ ") (" ++ go ns r ++ ")"
    go ns (Lam n (ExprScope inner)) =
        "λ" ++ varToString (head ns') ++ "." ++ go ns' inner
        where ns' = freeName n ns

var :: VarName a => P.Parsec String () a
var = do
    firstLetter      <- letter
    followingLetters <- many alphaNum
    return $ fromString (firstLetter : followingLetters)

lambda :: VarName a => P.Parsec String () (Expr a)
lambda = do
    char '\\'
    spaces
    bound <- var
    spaces
    char '.'
    mkLam bound <$> region

inRegion :: VarName a => P.Parsec String () (Expr a)
inRegion = between (char '(') (char ')') region <|> Free <$> var <|> lambda

region :: VarName a => P.Parsec String () (Expr a)
region = foldl1 App <$> sepBy inRegion spaces
