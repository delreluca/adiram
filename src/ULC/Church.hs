module ULC.Church where

import           ULC                            ( mkLam
                                                , Expr(Free, App)
                                                )

-- | Creates a Church encoded numeral from a Haskell integral.
churchNumeral :: (Eq a, Integral b) => a -> a -> b -> Expr a
churchNumeral s z n = mkLam s $ mkLam z $ go n
  where
    go l | l <= 0 = Free z
    go l          = Free s `App` go (pred l)
