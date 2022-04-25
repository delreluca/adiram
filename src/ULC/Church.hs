module ULC.Church (churchNumeral, foldChurchNumeral, churchList) where

import Protolude
import ULC
  ( Expr (App, Free, Lam),
    mapNames,
    mkLam,
    names,
    reifyLam,
  )

-- | Creates a Church encoded numeral from a Haskell integral.
churchNumeral ::
  (Eq a, Integral b) =>
  a -> -- The name of the `succ` binder
  a -> -- The name of the `zero` binder
  b -> -- The number to encode
  Expr a
churchNumeral s z = mkLam s . mkLam z . go
  where
    go n | n <= 0 = Free z
    go n = Free s `App` go (pred n)

data FoldChurchNumeralNames a = Succ | Zero | Original a

-- | Applies a function on an initial element according to a Church encoded numeral
foldChurchNumeral ::
  n -> -- The initial element
  (n -> n) -> -- The successor function
  Expr a -> -- The expression containing a Church encoded numeral
  Maybe n -- The iterated successor application on the initial element (if the expression contained a Church encoded numeral)
foldChurchNumeral z s e = do
  noS <- unpack (mapNames Original e) Succ
  noZ <- unpack noS Zero
  agg noZ
  where
    unpack l@(Lam _ _) a = Just $ reifyLam l (Free a)
    unpack _ _ = Nothing
    agg (App (Free Succ) d) = s <$> agg d
    agg (Free Zero) = Just z
    agg _ = Nothing

-- | Creates a Church encoded list from a Haskell foldable.
churchList ::
  (Eq a, Foldable t) =>
  -- | The name of the `cons` binder
  a ->
  -- | The name of the `nil` binder
  a ->
  -- | A name provider in case of clashes (receives preferred name and taken names)
  (a -> [a] -> a) ->
  -- | The members of the list to encode
  t (Expr a) ->
  Expr a
churchList c n namer es =
  mkLam c' . mkLam n' $
    foldr
      (\e -> (Free c' `App` e `App`))
      (Free n')
      es
  where
    c' = namer c inUse
    n' = namer n inUse
    inUse = foldr (\e a -> a ++ names e) [] es
