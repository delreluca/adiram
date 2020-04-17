module ULC.Church where

import           Protolude
import           ULC                            ( mkLam
                                                , names
                                                , Expr(Free, App)
                                                )

-- | Creates a Church encoded numeral from a Haskell integral.
churchNumeral :: (Eq a, Integral b) => a -> a -> b -> Expr a
churchNumeral s z = mkLam s . mkLam z . go
 where
  go n | n <= 0 = Free z
  go n          = Free s `App` go (pred n)

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
