{-# LANGUAGE NamedFieldPuns #-}

{-|
Description: Test Post Parsers using HUnit Testing Framework.

Module containing test cases for Post Parsers.

-}

module RequirementTests.PostParserTests
( postTestSuite ) where

import qualified Data.Text as T
import Database.DataType (PostType (..))
import Database.Tables
import Test.HUnit (Test (..), assertEqual)
import qualified Text.Parsec as Parsec
import Text.Parsec.Text (Parser)
import WebParsing.PostParser (postInfoParser)

-- Function to facilitate test case creation given a string, Req tuple
createTest :: Parser Post -> String -> [(T.Text, (T.Text, T.Text, PostType))] -> Test
createTest parser label input = TestLabel label $ TestList $ map (\(x, y) ->
                                TestCase $ assertEqual ("for (" ++ T.unpack x ++ "),")
                                (Right $ makePost y) (Parsec.parse parser "" x)) input

-- | Input and output pair of each post
-- | Output is in the order of (postDepartment, postCode, postName)
postInfoInputs :: [(T.Text, (T.Text, T.Text, PostType))]
postInfoInputs = [
      ("Music Major (Arts Program) - ASMAJ2276",
        ("Music Major (Arts Program)", "ASMAJ2276", Major))
    , ("Focus in Artificial Intelligence (Major) - ASFOC1689K",
        ("Focus in Artificial Intelligence (Major)", "ASFOC1689K", Focus))
    , ("Cell & Molecular Biology Specialist: Focus in Molecular Networks of the Cell - ASSPE1003A",
        ("Cell & Molecular Biology Specialist: Focus in Molecular Networks of the Cell", "ASSPE1003A", Specialist))
    , ("Focus in Medical Anthropology (Specialist: Society, Culture and Language) - ASFOC2112B",
        ("Focus in Medical Anthropology (Specialist: Society, Culture and Language)", "ASFOC2112B", Focus))
    , ("Anthropology Specialist (Society, Culture, and Language) (Arts Program) - ASSPE2112",
        ("Anthropology Specialist (Society, Culture, and Language) (Arts Program)", "ASSPE2112", Specialist))
    , ("Christianity and Culture: Major Program in Religious Education (Arts Program) - ASMAJ1021",
        ("Christianity and Culture: Major Program in Religious Education (Arts Program)", "ASMAJ1021", Major))
    , ("Minor in French Language (Arts Program) - ASMIN0120",
        ("Minor in French Language (Arts Program)", "ASMIN0120", Minor))
    , ("Minor Program in Christianity and Education (Arts Program) - ASMIN1014",
        ("Minor Program in Christianity and Education (Arts Program)", "ASMIN1014", Minor))
    , ("Psychology Research Specialist - Thesis (Science Program) - ASSPE1958)",
        ("Psychology Research Specialist - Thesis (Science Program)", "ASSPE1958", Specialist))
    , ("Cognitive Science Major - Arts (Language and Cognition Stream) (Arts Program) - ASMAJ1445B",
        ("Cognitive Science Major - Arts (Language and Cognition Stream) (Arts Program)", "ASMAJ1445B", Major))
    , ("Certificate in Business Fundamentals - ASCER2400",
        ("Certificate in Business Fundamentals", "ASCER2400", Certificate))
    , ("Focus in Finance - ASFOC2431B",
        ("Focus in Finance", "ASFOC2431B", Focus))
    , ("Focus in Green Chemistry",
        ("Focus in Green Chemistry", "", Focus))
    , ("Biological Physics Specialist",
        ("Biological Physics Specialist", "", Specialist))
    ]

-- | Takes a tuple of (postDepartment, postCode, postName) and makes a Post data
-- | by filling postDescription and postRequirements with empty text
makePost :: (T.Text, T.Text, PostType) -> Post
makePost (postDepartment, postCode, postName) =
    Post { postName, postDepartment, postCode, postDescription = T.empty, postRequirements = T.empty }

postInfoTests :: Test
postInfoTests = createTest postInfoParser "Post requirements" postInfoInputs

-- functions for running tests in REPL
postTestSuite :: Test
postTestSuite = TestLabel "PostParser tests" $ TestList [postInfoTests]
