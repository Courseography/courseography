{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

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
createTest :: (Eq a, Show a) => Parser a -> String -> [(T.Text, a)] -> Test
createTest parser label input = TestLabel label $ TestList $ map (\(x, y) ->
                                TestCase $ assertEqual ("for (" ++ T.unpack x ++ "),")
                                (Right y) (Parsec.parse parser "" x)) input

postInfoInputs :: [(T.Text, Post)]
postInfoInputs = [
      ("Music Major (Arts Program) - ASMAJ2276", Post {postName = Major, postDepartment = "Music (Arts Program)", postCode = "ASMAJ2276", postDescription = ""})
    , ("Focus in Artificial Intelligence (Major) - ASFOC1689K", Post {postName = Major, postDepartment = "Focus in Artificial Intelligence", postCode = "ASFOC1689K", postDescription = ""})
    , ("Cell & Molecular Biology Specialist: Focus in Molecular Networks of the Cell - ASSPE1003A", Post {postName = Specialist, postDepartment = "Cell & Molecular Biology Focus in Molecular Networks of the Cell", postCode = "ASSPE1003A", postDescription = ""})
    , ("Focus in Medical Anthropology (Specialist: Society, Culture and Language) - ASFOC2112B", Post {postName = Specialist, postDepartment = "Focus in Medical Anthropology (Society, Culture and Language)", postCode = "ASFOC2112B", postDescription = ""})
    , ("Anthropology Specialist (Society, Culture, and Language) (Arts Program) - ASSPE2112", Post {postName = Specialist, postDepartment = "Anthropology (Society, Culture, and Language) (Arts Program)", postCode = "ASSPE2112", postDescription = ""})
    , ("Christianity and Culture: Major Program in Religious Education (Arts Program) - ASMAJ1021", Post {postName = Major, postDepartment = "Christianity and Culture: Religious Education (Arts Program)", postCode = "ASMAJ1021", postDescription = ""})
    , ("Minor in French Language (Arts Program) - ASMIN0120", Post {postName = Minor, postDepartment = "French Language (Arts Program)", postCode = "ASMIN0120", postDescription = ""})
    , ("Minor Program in Christianity and Education (Arts Program) - ASMIN1014", Post {postName = Minor, postDepartment = "Christianity and Education (Arts Program)", postCode = "ASMIN1014", postDescription = ""})
    , ("Psychology Research Specialist - Thesis (Science Program) - ASSPE1958)", Post {postName = Specialist, postDepartment = "Psychology Research - Thesis (Science Program)", postCode = "ASSPE1958", postDescription = ""})
    , ("Cognitive Science Major - Arts (Language and Cognition Stream) (Arts Program) - ASMAJ1445B", Post {postName = Major, postDepartment = "Cognitive Science - Arts (Language and Cognition Stream) (Arts Program)", postCode = "ASMAJ1445B", postDescription = ""})
    , ("Certificate in Business Fundamentals - ASCER2400", Post {postName = Certificate, postDepartment = "Business Fundamentals", postCode = "ASCER2400", postDescription = ""})
    , ("Focus in Green Chemistry", Post {postName = Other, postDepartment = "Focus in Green Chemistry", postCode = "", postDescription = ""})
    , ("Focus in Finance - ASFOC2431B", Post {postName = Other, postDepartment = "Focus in Finance", postCode = "ASFOC2431B", postDescription = ""})
    , ("Biological Physics Specialist", Post {postName = Specialist, postDepartment = "Biological Physics", postCode = "", postDescription = ""})
    ]
postInfoTests :: Test
postInfoTests = createTest postInfoParser "Post requirements" postInfoInputs

-- functions for running tests in REPL
postTestSuite :: Test
postTestSuite = TestLabel "PostParser tests" $ TestList [postInfoTests]
