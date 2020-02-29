import           Test.Tasty
import           Test.Tasty.HUnit
import           ULC
import           ULC.Parser
import qualified Text.Parsec                   as P

main = defaultMain tests

tests :: TestTree
tests = testGroup
    "All tests"
    [ulcId, ulcChurchZero, ulcChurchOne, ulcComplex, ulcPrettyPrint, ulcChurchSugar]

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
--Church list expressions
cNil = mkLam "c" $ mkLam "n" $ Free "n" 
cSingleList e = mkLam "c" $ mkLam "n" $ Free "c" `App` e `App` Free "n"
cDoubleList e1 e2 = mkLam "c" $ mkLam "n" $ Free "c" `App` e1 `App` (Free "c" `App` e2 `App` Free "n")

ulcComplex :: TestTree
ulcComplex = ulcParsing
    (padCombinations 1 "(\\s.\\z.s z) (\\c.\\s.\\z.s (c s z)) \\s.\\z.z")
    ((c1 `App` cScc) `App` c0)

ulcChurchSugar :: TestTree
ulcChurchSugar = testGroup "Syntactic sugar for Church numerals and lists"
                           [
                               ulcParsing ["0"] c0,
                               ulcParsing ["1"] c1,
                               ulcParsing ["[0]"] $ cSingleList c0,
                               ulcParsing ["[]"] cNil,
                               ulcParsing ["[a,1]"] $ cDoubleList (Free "a") c1]

-- | Test cases for the untyped lambda calculus parser.
ulcParsing :: [String] -> Expr String -> TestTree
ulcParsing ss expr = testGroup
    ("Expecting to parse " ++ prettyPrint expr)
    (   (\s ->
            testCase ("Input >" ++ s ++ "<")
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
