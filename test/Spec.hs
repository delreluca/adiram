{-#LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit
import           ULC
import qualified ULC2.Parsing                  as Nu
import qualified Text.Parsec                   as P

main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [ulcId, ulcChurchZero, ulcChurchOne, ulcComplex]

-- | Adds combinations of whitespace in front and or after a string.
padCombinations :: Int -> String -> [String]
padCombinations n s =
    let spaces = [ replicate i ' ' | i <- [0..n] ]
    in  do
            left  <- spaces
            right <- spaces
            return $ left ++ s ++ right

ulcId :: TestTree
ulcId = ulcParsing (padCombinations 2 "\\x.x") (mkLam "x" $ Free "x")

ulcChurchZero :: TestTree
ulcChurchZero =
    let combos = do
            z <- padCombinations 2 "z"
            zz <- padCombinations 1 $ "\\z." ++ z
            padCombinations 2 $ "\\s." ++ zz
    in ulcParsing combos (mkLam "s" $ mkLam "z" $ Free "z")

ulcChurchOne :: TestTree
ulcChurchOne = 
    let combos = do
            s <- padCombinations 1 "s"
            z <- padCombinations 1 " z"
            sz <- padCombinations 1 $ s ++ z
            zz <- padCombinations 1 $ "\\z." ++ sz
            padCombinations 1 $ "\\s." ++ zz
    in ulcParsing combos (mkLam "s" $ mkLam "z" $ App (Free "s") (Free "z"))

ulcComplex :: TestTree
ulcComplex =
    let c1 = mkLam "s" $ mkLam "z" $ Free "s" `App` Free "z"
        c0 = mkLam "s" $ mkLam "z" $ Free "z"
        succ = mkLam "c" $ mkLam "s" $ mkLam "z" $ Free "s" `App` ((Free "c" `App` Free "s") `App` Free "z")
    in ulcParsing (padCombinations 1 "(\\s.\\z.s z) (\\c.\\s.\\z.s (c s z)) \\s.\\z.z") ((c1 `App` succ) `App` c0)

-- | Test cases for the untyped lambda calculus parser.
ulcParsing :: [String] -> Expr String -> TestTree
ulcParsing ss expr = testGroup
    "Untyped lambda calculus parsing"
    (   (\s ->
            testCase ("ULC parsing [" ++ s ++ "]")
                $ let res =
                          case
                                  P.parse
                                      (Nu.term0 :: P.Parsec String () (Expr String)
                                      )
                                      ""
                                      s
                              of
                                  Right a -> a
                                  Left  e -> error $ "Parsec error: " ++ show e
                  in  res @?= expr
        )
    <$> ss
    )