import           Test.Tasty
import           Test.Tasty.HUnit
import           ULC
import qualified Text.Parsec                   as P

main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [ulcId, ulcChurchZero, ulcChurchOne, ulcComplex, ulcPrettyPrint]

-- | Adds combinations of whitespace in front and or after a string.
padCombinations :: Int -> String -> [String]
padCombinations n s =
    let spaces = [ replicate i ' ' | i <- [0 .. n] ]
    in  do
            left  <- spaces
            right <- spaces
            return $ left ++ s ++ right

ulcId :: TestTree
ulcId = ulcParsing (padCombinations 2 "\\x.x") (mkLam "x" $ Free "x")

ulcChurchZero :: TestTree
ulcChurchZero =
    let combos = do
            z  <- padCombinations 2 "z"
            zz <- padCombinations 1 $ "\\z." ++ z
            padCombinations 2 $ "\\s." ++ zz
    in  ulcParsing combos c0

ulcChurchOne :: TestTree
ulcChurchOne =
    let combos = do
            s  <- padCombinations 1 "s"
            z  <- padCombinations 1 " z"
            sz <- padCombinations 1 $ s ++ z
            zz <- padCombinations 1 $ "\\z." ++ sz
            padCombinations 1 $ "\\s." ++ zz
    in  ulcParsing combos c1

-- Church numeral expressions
c0 = mkLam "s" $ mkLam "z" $ Free "z"
c1 = mkLam "s" $ mkLam "z" $ Free "s" `App` Free "z"
cScc =
    mkLam "c"
        $     mkLam "s"
        $     mkLam "z"
        $     Free "s"
        `App` ((Free "c" `App` Free "s") `App` Free "z")

ulcComplex :: TestTree
ulcComplex = ulcParsing
    (padCombinations 1 "(\\s.\\z.s z) (\\c.\\s.\\z.s (c s z)) \\s.\\z.z")
    ((c1 `App` cScc) `App` c0)

-- | Test cases for the untyped lambda calculus parser.
ulcParsing :: [String] -> Expr String -> TestTree
ulcParsing ss expr = testGroup
    ("Untyped lambda calculus parser tests for " ++ prettyPrint expr)
    (   (\s ->
            testCase ("ULC parsing [" ++ s ++ "]")
                $ let res = case P.parse parser "" s of
                          Right a -> a
                          Left  e -> error $ "Parsec error: " ++ show e
                  in  res @?= expr
        )
    <$> ss
    )

ulcPrettyPrint :: TestTree
ulcPrettyPrint = testGroup
    "ULC pretty print"
    [ testCase "Id" $ prettyPrint (mkLam "x" $ Free "x") @?= "λx.x"
    , testCase "c1" $ prettyPrint c1 @?= "λs.λz.s z"
    , testCase "c1 cScc c0"
    $   prettyPrint (c1 `App` cScc `App` c0)
    @?= "((λs.λz.s z) λc.λs.λz.s (c s z)) λs.λz.z"
    ]
