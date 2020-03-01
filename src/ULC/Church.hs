module ULC.Church where

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
churchList :: (Eq a, Foldable t) => a -> a -> (a -> [a] -> a) -> t (Expr a) -> Expr a
churchList c n namer es = mkLam c' . mkLam n' $ foldr (\e -> (Free c' `App` e `App`)) (Free n') es
  where c' = namer c inUse
        n' = namer n inUse
        inUse = foldr (\e a -> a ++ names e) [] es