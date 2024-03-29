import Data.Text
  ( replicate,
    unpack,
  )
import Protolude hiding (replicate)
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Parsec as P
import ULC
import ULC.Church (foldChurchNumeral)
import ULC.Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "All tests"
    [ ulcId,
      ulcChurchZero,
      ulcChurchOne,
      ulcComplex,
      ulcPrettyPrint,
      ulcChurchSugar,
      ulcChurchNumeralToInteger
    ]

-- | Adds combinations of whitespace in front and or after a string.
padCombinations :: Int -> Text -> [Text]
padCombinations n s =
  let spaces = [replicate i " " | i <- [0 .. n]]
   in do
        left <- spaces
        right <- spaces
        return $ left <> s <> right

ulcId :: TestTree
ulcId = ulcParsing (padCombinations 2 "\\x.x") (mkLam "x" $ Free "x")

ulcChurchZero :: TestTree
ulcChurchZero =
  let combos = do
        z <- padCombinations 2 "z"
        zz <- padCombinations 1 $ "\\z." <> z
        padCombinations 2 $ "\\s." <> zz
   in ulcParsing combos c0

ulcChurchOne :: TestTree
ulcChurchOne =
  let combos = do
        s <- padCombinations 1 "s"
        z <- padCombinations 1 " z"
        sz <- padCombinations 1 $ s <> z
        zz <- padCombinations 1 $ "\\z." <> sz
        padCombinations 1 $ "\\s." <> zz
   in ulcParsing combos c1

-- Church numeral expressions
c0, c1, c3, cScc :: Expr Text
c0 = mkLam "s" $ mkLam "z" $ Free "z"
c1 = mkLam "s" $ mkLam "z" $ Free "s" `App` Free "z"
c3 = mkLam "s" $ mkLam "z" $ Free "s" `App` (Free "s" `App` (Free "s" `App` Free "z"))
cScc =
  mkLam "c" $
    mkLam "s" $
      mkLam "z" $
        Free "s"
          `App` ((Free "c" `App` Free "s") `App` Free "z")

--Church list expressions
cNil, cNameClash :: Expr Text
cNil = mkLam "c" $ mkLam "n" $ Free "n"
cNameClash =
  mkLam "c'" $
    mkLam "n'" $
      Free "c'"
        `App` Free "c"
        `App` (Free "c'" `App` Free "n" `App` Free "n'")

cSingleList :: Expr Text -> Expr Text
cSingleList e = mkLam "c" $ mkLam "n" $ Free "c" `App` e `App` Free "n"

cDoubleList :: Expr Text -> Expr Text -> Expr Text
cDoubleList e1 e2 =
  mkLam "c" $
    mkLam "n" $
      Free "c"
        `App` e1
        `App` (Free "c" `App` e2 `App` Free "n")

ulcComplex :: TestTree
ulcComplex =
  ulcParsing
    (padCombinations 1 "(\\s.\\z.s z) (\\c.\\s.\\z.s (c s z)) \\s.\\z.z")
    ((c1 `App` cScc) `App` c0)

ulcChurchSugar :: TestTree
ulcChurchSugar =
  testGroup
    "Syntactic sugar for Church numerals and lists"
    [ ulcParsing ["0"] c0,
      ulcParsing ["1"] c1,
      ulcParsing ["[0]"] $ cSingleList c0,
      ulcParsing ["[]"] cNil,
      ulcParsing ["[a,1]"] $ cDoubleList (Free "a") c1,
      ulcParsing ["[c,n]"] cNameClash
    ]

ulcChurchNumeralToInteger :: TestTree
ulcChurchNumeralToInteger =
  testGroup
    "Folding over a Church numeral to get a Haskell integer"
    [ checkInteger 0 c0,
      checkInteger 1 c1,
      checkInteger 3 c3,
      testCase "scc should not evaluate" (foldChurchNumeral (0 :: Integer) succ cScc @?= Nothing)
    ]
  where
    checkInteger n c = testCase ("Evaluating Church numeral " ++ show n) (foldChurchNumeral (0 :: Integer) succ c @?= Just n)

-- | Test cases for the untyped lambda calculus parser.
ulcParsing :: [Text] -> Expr Text -> TestTree
ulcParsing ss expr =
  testGroup
    (unpack $ "Expecting to parse " <> prettyPrint expr)
    ( ( \s ->
          testCase (unpack ("Input >" <> s <> "<")) $
            let res = case P.parse parser "" s of
                  Right a -> a
                  Left e ->
                    panic $
                      "Parsec error: "
                        <> show e
             in res @?= expr
      )
        <$> ss
    )

ulcPrettyPrint :: TestTree
ulcPrettyPrint =
  testGroup
    "ULC pretty print"
    [ testCase "Id" $ prettyPrint (mkLam "x" $ Free "x") @?= "λx.x",
      testCase "c1" $ prettyPrint c1 @?= "λs.λz.s z",
      testCase "c1 cScc c0" $
        prettyPrint (c1 `App` cScc `App` c0)
          @?= "((λs.λz.s z) λc.λs.λz.s (c s z)) λs.λz.z"
    ]
