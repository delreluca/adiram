{-#LANGUAGE OverloadedStrings #-}

import           Test.Tasty
import           Test.Tasty.HUnit
import           ULC
import qualified Text.Parsec                   as P

main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [ulcParsing]

-- | Adds combinations of whitespace in front and or after a string.
padCombinations :: String -> [String]
padCombinations s =
    let spaces = ["", " ", "  "]
    in do
        left <- spaces
        right <- spaces
        return $ left ++ s ++ right

-- | Test cases for the untyped lambda calculus parser.
ulcParsing :: TestTree
ulcParsing = testGroup
    "Untyped lambda calculus parsing"
    ((\s ->
    testCase ("ULC parsing [" ++ s ++ "]")
          $ let
                res =
                    case
                            P.parse
                                (region :: P.Parsec String () (Expr ApoString))
                                ""
                                s
                        of
                            Right a -> a
                            Left  e -> error $ "Parsec error: " ++ show e
                x = "x" :: ApoString
            in
                res @?= mkLam x (Free x)
    ) <$> padCombinations "\\x.x")