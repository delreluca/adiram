module ULC
  ( ExprScope()
  , Expr(Free, App, Lam)
  , mkLam
  , showExpr
  , reduceCbv
  , reduceNormal
  , parser
  )
where

import           Data.Functor                   ( ($>) )
import           Text.Parsec                    ( (<|>)
                                                , eof
                                                , spaces
                                                , char
                                                , letter
                                                , alphaNum
                                                , many
                                                , chainl1
                                                , Parsec
                                                )

-- | A scoped expression that contains bound variables; no value constructor is exported, use 'mkLam' to create lambda abstractions.
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

freeName :: String -> [String] -> [String]
freeName n ns | n `notElem` ns = n : ns
freeName n ns                  = freeName (n ++ "'") ns

-- | Prints a lambda expression; not pretty
showExpr :: Expr String -> String
showExpr = go []
 where
  go _  (Free  a                ) = a
  go ns (Bound i                ) = ns !! i
  go ns (App l r                ) = "(" ++ go ns l ++ ") (" ++ go ns r ++ ")"
  go ns (Lam n (ExprScope inner)) = "λ" ++ head ns' ++ "." ++ go ns' inner
    where ns' = freeName n ns

-- | Shifting of indices with cutoffs to protect bound variables in current scope
-- See also Pierce chapter 6
shift :: Int -> Int -> Expr a -> Expr a
shift _ _ x@(Free _) = x
shift by cutoff x@(Bound l) | l < cutoff = x
                            | otherwise  = Bound (l + by)
shift by cutoff (App l r) = shift by cutoff l `App` shift by cutoff r
shift by cutoff (Lam a (ExprScope inner)) =
  Lam a (ExprScope (shift by (succ cutoff) inner))

-- | Default shift by 1 and cutoff 0
shift10 = shift 1 0

-- | Performs a substitution of bound variables
subst :: Int -> Expr a -> Expr a -> Expr a
subst _ _ x@(Free _) = x
subst i by x@(Bound j) | i == j    = by
                       | otherwise = x
subst i by (App l r) = subst i by l `App` subst i by r
subst i by (Lam a (ExprScope inner)) =
  Lam a (ExprScope (subst (succ i) (shift10 by) inner))

-- | Performs the substiution of the App-Abs evaluation rule
substAppAbs :: Expr a -> Expr a -> Expr a
substAppAbs by scoped = shift (-1) 0 (subst 0 (shift10 by) scoped)

-- | Determines whether an expression is a value, i.e. cannot be reduced anymore
-- Since we use free variables in addition to de Brujin indices, we well consider those values as well
isValue :: Expr a -> Bool
isValue (Free _ ) = True
isValue (Lam _ _) = True
isValue _         = False

-- | Performs one step of a reduction using call-by-value.
-- It only reduces the outermost, leftmost redex if its right hand side is a value. Otherwise it reduces the right hand side first.
reduce1Cbv :: Expr a -> Maybe (Expr a)
reduce1Cbv (App (Lam _ (ExprScope inner)) value) | isValue value =
  Just $ substAppAbs value inner
reduce1Cbv (App value next) | isValue value = App value <$> reduce1Cbv next
reduce1Cbv (App next later)                 = flip App later <$> reduce1Cbv next
reduce1Cbv _                                = Nothing

-- | Reduces an expression using normal order.
-- It reduces the outermost, leftmost redex.
reduce1Normal :: Expr a -> Maybe (Expr a)
reduce1Normal (App (Lam _ (ExprScope inner)) rhs) =
  Just $ substAppAbs rhs inner
reduce1Normal (App lhs rhs) = case reduce1Normal lhs of
                                Just lhs' -> Just $ App lhs' rhs
                                Nothing -> App lhs <$> reduce1Normal rhs
reduce1Normal (Lam v (ExprScope inner)) = Lam v . ExprScope <$> reduce1Normal inner
reduce1Normal _ = Nothing

-- | Reduces an expression step by step as long as an evaluation step is possible.
reduce :: (Expr a -> Maybe (Expr a)) -> Expr a -> Expr a
reduce t e = maybe e (reduce t) $ t e

-- | Reduces an expression using call-by-value.
reduceCbv = reduce reduce1Cbv

-- | Reduces an expression using normal order.
reduceNormal = reduce reduce1Normal

-- Parsing
name = (:) <$> letter <*> many alphaNum
free = Free <$> name
abst = mkLam <$> (char '\\' *> name <* char '.') <*> term
appl = spaces $> App
paren = char '(' *> term <* char ')'
token = spaces *> (free <|> abst <|> paren) <* spaces
term = chainl1 token appl

-- | Parses a full (i.e. awaiting EOF at the end) expression in the untyped lambda calculus
parser :: Parsec String () (Expr String)
parser = term <* eof
