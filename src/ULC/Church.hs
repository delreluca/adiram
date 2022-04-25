module ULC.Church (churchNumeral, foldChurchNumeral, churchList) where

import           Protolude
import           ULC                            ( mkLam
                                                , names
                                                , Expr(Free, App, Lam), reifyLam, mapNames
                                                )

-- | Creates a Church encoded numeral from a Haskell integral.
churchNumeral :: (Eq a, Integral b) => a -> a -> b -> Expr a
churchNumeral s z = mkLam s . mkLam z . go
 where
  go n | n <= 0 = Free z
  go n          = Free s `App` go (pred n)

data FoldChurchNumeralNames a = Succ | Zero | Original a

-- | Applies a function on an initial element according to a Church encoded numeral
foldChurchNumeral
  :: n -- The initial element
  -> (n -> n) -- The successor function
  -> Expr a -- The expression containing a Church encoded numeral
  -> Maybe n -- The iterated successor application on the initial element (if the expression contained a Church encoded numeral)
foldChurchNumeral z s e = do
  noS <- unpack (mapNames Original e) Succ
  noZ <- unpack noS Zero
  agg noZ
  where unpack l@(Lam _ _) a = Just $ reifyLam l (Free a)
        unpack _ _ = Nothing
        agg (App (Free Succ) d) = s <$> agg d
        agg (Free Zero) = Just z
        agg _ = Nothing

-- | Creates a Church encoded list from a Haskell foldable.
churchList
  :: (Eq a, Foldable t)
  => a -- ^ name of the cons binder
  -> a -- ^ name of the nil binder
  -> (a -> [a] -> a) -- ^ name provider in case of clashes (receives preferred name and taken names)
  -> t (Expr a) -- ^ members of the list
  -> Expr a
churchList c n namer es = mkLam c' . mkLam n' $ foldr
  (\e -> (Free c' `App` e `App`))
  (Free n')
  es
 where
  c'    = namer c inUse
  n'    = namer n inUse
  inUse = foldr (\e a -> a ++ names e) [] es
