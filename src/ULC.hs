module ULC
  ( ExprScope (),
    Expr (Free, App, Lam),
    Environment (Environment),
    freeVars,
    iterationLimit,
    freeName,
    mkLam,
    reifyLam,
    names,
    prettyPrint,
    reduceCbv,
    reduceNormal,
    stripNames,
    mapNames,
  )
where

import qualified Data.Map as M
import Protolude hiding
  ( reduce,
    shift,
  )

-- | A scoped expression that contains bound variables; no value constructor is exported, use 'mkLam' to create lambda abstractions.
newtype ExprScope a = ExprScope (Expr a)
  deriving (Eq, Show)

-- | Represents an expression in the untyped lambda calculucs, where de Brujin indices are used for bound variables.
-- Note that 'Bound' is not exported from this module, use 'mkLam' to create lambda abstractions binding variables.
data Expr a
  = -- | A free variable
    Free a
  | -- | A bound variable containing its de Brujin index
    Bound !Int
  | -- | An application of a right to a left expression
    App (Expr a) (Expr a)
  | -- | An abstraction with a scoped inner term with bound variables and a preferred name for it
    Lam a (ExprScope a)
  deriving (Eq, Show)

-- | Replaces the preferred names in lambda abstractions with a static name.
-- This is useful for comparing expressions without comparing preferred names.
stripNames :: a -> Expr a -> Expr a
stripNames a (Lam _ (ExprScope i)) = Lam a (ExprScope (stripNames a i))
stripNames a (App x y) = stripNames a x `App` stripNames a y
stripNames _ x = x

-- | Maps names of free variables and preferred names of bound variables.
-- This is useful to enlargen the type of the names.
mapNames :: (a -> b) -> Expr a -> Expr b
mapNames f (Lam a (ExprScope i)) = Lam (f a) (ExprScope (mapNames f i))
mapNames f (App x y) = mapNames f x `App` mapNames f y
mapNames f (Free a) = Free $ f a
mapNames _ (Bound i) = Bound i

-- | Takes an expression and turns it into a scope by binding a given free variable.
abstract :: Eq a => a -> Expr a -> ExprScope a
abstract nameToBind e = ExprScope $ go 0 e
  where
    go _ (Bound i) = Bound i
    go level (Free a)
      | nameToBind == a = Bound level
      | otherwise = Free a
    go level (App l r) = go level l `App` go level r
    go level (Lam a (ExprScope inner)) =
      Lam a $ ExprScope $ go (succ level) inner

-- | Returns a lambda abstraction by binding a given free variable and using it as preferred name
mkLam :: Eq a => a -> Expr a -> Expr a
mkLam a = Lam a . abstract a

-- | Undoes a lambda abstraction by substituting the bound variable. The result is guaranteed to not be an abstraction.
reifyLam ::
  -- | The expression to reify
  Expr a ->
  -- | The substitute for the bound variable
  Expr a ->
  -- | The reified expression or, in case it was not a lambda abstraction, the original expression
  Expr a
reifyLam (Lam _ (ExprScope inner)) by = substAppAbs by inner
reifyLam e _ = e

-- | Returns the free (unbound) variables in an expression. The result might contain duplicates.
names :: Expr a -> [a]
names (Bound _) = []
names (Free n) = [n]
names (a `App` b) = names a ++ names b
names (Lam _ (ExprScope a)) = names a

-- | Finds a name that is not used by a free variable yet.
freeName :: Text -> [Text] -> [Text]
freeName n ns | n `notElem` ns = n : ns
freeName n ns = freeName (n <> "'") ns

-- | Prints an expression in the untyped lambda calculus. Collisions in bound variables are resolved by apostrophe suffixes.
-- Tries to reduce the number of parantheses by only using them to preserve application order and preventing suffxing to abstractions.
prettyPrint :: Expr Text -> Text
prettyPrint = go []
  where
    go _ (Free a) = a
    go ns (Bound i) = fromMaybe "??" (ns `atMay` i)
    go ns (App l r) = lhs ns l <> " " <> rhs ns r
      where
        lhs ns' x@(Lam _ _) = "(" <> go ns' x <> ")" -- it is not safe to append to an abstraction to the right
        lhs ns' x@(App _ (Lam _ _)) = "(" <> go ns' x <> ")" -- same if it is the right hand side of an application
        lhs ns' x = go ns' x
        rhs ns' x@(App _ _) = "(" <> go ns' x <> ")" -- appending an application without parentheses will change order
        rhs ns' x = go ns' x
    go ns (Lam n (ExprScope t)) = "λ" <> headDef "??" ns' <> "." <> go ns' t
      where
        ns' = freeName n ns

-- | Shifting of indices with cutoffs to protect bound variables in current scope
-- See also Pierce chapter 6
shift :: Int -> Int -> Expr a -> Expr a
shift _ _ x@(Free _) = x
shift by cutoff x@(Bound l)
  | l < cutoff = x
  | otherwise = Bound (l + by)
shift by cutoff (App l r) = shift by cutoff l `App` shift by cutoff r
shift by cutoff (Lam a (ExprScope inner)) =
  Lam a (ExprScope (shift by (succ cutoff) inner))

shiftInc :: Expr a -> Expr a
shiftInc = shift 1 0

shiftDec :: Expr a -> Expr a
shiftDec = shift (-1) 0

-- | Performs a substitution of bound variables
subst :: Int -> Expr a -> Expr a -> Expr a
subst _ _ x@(Free _) = x
subst i by x@(Bound j)
  | i == j = by
  | otherwise = x
subst i by (App l r) = subst i by l `App` subst i by r
subst i by (Lam a (ExprScope inner)) =
  Lam a (ExprScope (subst (succ i) (shiftInc by) inner))

-- | Performs the substiution of the App-Abs evaluation rule
substAppAbs :: Expr a -> Expr a -> Expr a
substAppAbs by scoped = shiftDec (subst 0 (shiftInc by) scoped)

-- | Determines whether an expression is a value, i.e. cannot be reduced anymore
-- In order to determine whether a free variable is a value the environment is needed.
extractValue :: Ord a => Environment a -> Expr a -> Maybe (Expr a)
extractValue g (Free n) = M.lookup n (freeVars g) >>= extractValue g
extractValue _ (Lam _ (ExprScope i)) = Just i
extractValue _ _ = Nothing

-- | Performs one step of a reduction using call-by-value.
-- It only reduces the outermost, leftmost redex if its right hand side is a value. Otherwise it reduces the right hand side first.
reduce1Cbv :: Ord a => Environment a -> Expr a -> Maybe (Expr a)
reduce1Cbv g (App l r) = case (extractValue g l, extractValue g r) of
  (Just li, Just _) -> Just $ substAppAbs r li
  (Just _, Nothing) -> App l <$> reduce1Cbv g r
  (Nothing, _) -> flip App r <$> reduce1Cbv g l
reduce1Cbv g (Free n) = M.lookup n $ freeVars g
reduce1Cbv _ _ = Nothing

-- | Reduces an expression using normal order.
-- It reduces the outermost, leftmost redex.
reduce1Normal :: Ord a => Environment a -> Expr a -> Maybe (Expr a)
reduce1Normal _ (App (Lam _ (ExprScope inner)) rhs) =
  Just $ substAppAbs rhs inner
reduce1Normal g (App lhs rhs) = case reduce1Normal g lhs of
  Just lhs' -> Just $ App lhs' rhs
  Nothing -> App lhs <$> reduce1Normal g rhs
reduce1Normal g (Lam v (ExprScope inner)) =
  Lam v . ExprScope <$> reduce1Normal g inner
reduce1Normal g (Free n) = M.lookup n $ freeVars g
reduce1Normal _ _ = Nothing

-- | Reduces an expression step by step as long as an evaluation step is possible.
reduce ::
  (Environment a -> Expr a -> Maybe (Expr a)) ->
  -- | The environment containing free variables and iteration limits
  Environment a ->
  -- | The original expression
  Expr a ->
  Expr a
reduce t g = go $ iterationLimit g
  where
    go 0 x = x
    go n x = maybe x (go (pred n)) $ t g x

-- | Reduces an expression using call-by-value.
reduceCbv :: Ord a => Environment a -> Expr a -> Expr a
reduceCbv = reduce reduce1Cbv

-- | Reduces an expression using normal order.
reduceNormal :: Ord a => Environment a -> Expr a -> Expr a
reduceNormal = reduce reduce1Normal

-- | Contains the environment of out of scope free variables
data Environment a = Environment {freeVars :: M.Map a (Expr a), iterationLimit :: Int} deriving (Show, Eq)
