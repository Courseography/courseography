{-|
Description: Test Post Parsers using HUnit Testing Framework.

Module containing test cases for Post Parsers.

-}

module RequirementTests.PostParserTests
( postTestSuite ) where

import Data.Bifunctor (second)
import qualified Data.Text as T
import Database.DataType (PostType (..))
import Test.HUnit (Test (..), assertEqual)
import qualified Text.Parsec as Parsec
import WebParsing.PostParser (getPostType, postInfoParser)

-- Function to facilitate test case creation given a string, Req tuple
createTest :: (Show a, Eq a, Show b, Eq b) => (a -> b) -> String -> [(a, b)] -> Test
createTest function label input = TestLabel label $ TestList $ map (\(x, y) ->
                                TestCase $ assertEqual ("for (" ++ show x ++ "),")
                                y (function x)) input

-- | Input and output pair of each post
-- | Output is in the order of (postDepartment, postCode, postName)
postInfoInputs :: [(T.Text, (T.Text, T.Text))]
postInfoInputs = [
      ("Music Major (Arts Program) - ASMAJ2276",
        ("Music Major (Arts Program)", "ASMAJ2276"))
    , ("Focus in Artificial Intelligence (Major) - ASFOC1689K",
        ("Focus in Artificial Intelligence (Major)", "ASFOC1689K"))
    , ("Cell & Molecular Biology Specialist: Focus in Molecular Networks of the Cell - ASSPE1003A",
        ("Cell & Molecular Biology Specialist: Focus in Molecular Networks of the Cell", "ASSPE1003A"))
    , ("Focus in Medical Anthropology (Specialist: Society, Culture and Language) - ASFOC2112B",
        ("Focus in Medical Anthropology (Specialist: Society, Culture and Language)", "ASFOC2112B"))
    , ("Anthropology Specialist (Society, Culture, and Language) (Arts Program) - ASSPE2112",
        ("Anthropology Specialist (Society, Culture, and Language) (Arts Program)", "ASSPE2112"))
    , ("Christianity and Culture: Major Program in Religious Education (Arts Program) - ASMAJ1021",
        ("Christianity and Culture: Major Program in Religious Education (Arts Program)", "ASMAJ1021"))
    , ("Minor in French Language (Arts Program) - ASMIN0120",
        ("Minor in French Language (Arts Program)", "ASMIN0120"))
    , ("Minor Program in Christianity and Education (Arts Program) - ASMIN1014",
        ("Minor Program in Christianity and Education (Arts Program)", "ASMIN1014"))
    , ("Psychology Research Specialist - Thesis (Science Program) - ASSPE1958)",
        ("Psychology Research Specialist - Thesis (Science Program)", "ASSPE1958"))
    , ("Cognitive Science Major - Arts (Language and Cognition Stream) (Arts Program) - ASMAJ1445B",
        ("Cognitive Science Major - Arts (Language and Cognition Stream) (Arts Program)", "ASMAJ1445B"))
    , ("Certificate in Business Fundamentals - ASCER2400",
        ("Certificate in Business Fundamentals", "ASCER2400"))
    , ("Focus in Finance - ASFOC2431B",
        ("Focus in Finance", "ASFOC2431B"))
    , ("Focus in Green Chemistry",
        ("Focus in Green Chemistry", ""))
    , ("Biological Physics Specialist",
        ("Biological Physics Specialist", ""))
    ]

getPostTypeInputs :: [((T.Text, T.Text), PostType)]
getPostTypeInputs = [
      (("ASSPE1958", "Psychology Specialist"), Specialist)
    , (("ASMAJ2276", "Music Major"), Major)
    , (("ASMIN0120", "Minor in French"), Minor)
    , (("ASFOC1689B", "Focus in AI"), Focus)
    , (("ASCER2400", "Certificate in Business"), Certificate)
    , (("", "Psychology Specialist"), Specialist)
    , (("", "Music Major"), Major)
    , (("", "Minor in French"), Minor)
    , (("", "Focus in AI"), Focus)
    , (("", "Certificate in Business"), Certificate)
    ]

postInfoTests :: Test
postInfoTests = createTest (Parsec.parse postInfoParser "") "Post requirements" $ map (second Right) postInfoInputs

getPostTypeTests :: Test
getPostTypeTests = createTest (uncurry getPostType) "Post requirements" getPostTypeInputs

-- functions for running tests in REPL
postTestSuite :: Test
postTestSuite = TestLabel "PostParser tests" $ TestList [postInfoTests, getPostTypeTests]
