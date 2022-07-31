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
      (T.pack "Music Major (Arts Program) - ASMAJ2276", Post {postName = Major, postDepartment = T.pack "Music (Arts Program)", postCode = T.pack "ASMAJ2276", postDescription = T.pack ""})
    , (T.pack "Focus in Artificial Intelligence (Major) - ASFOC1689K", Post {postName = Major, postDepartment = T.pack "Focus in Artificial Intelligence", postCode = T.pack "ASFOC1689K", postDescription = T.pack ""})
    , (T.pack "Cell & Molecular Biology Specialist: Focus in Molecular Networks of the Cell - ASSPE1003A", Post {postName = Specialist, postDepartment = T.pack "Cell & Molecular Biology Focus in Molecular Networks of the Cell", postCode = T.pack "ASSPE1003A", postDescription = T.pack ""})
    , (T.pack "Focus in Medical Anthropology (Specialist: Society, Culture and Language) - ASFOC2112B", Post {postName = Specialist, postDepartment = T.pack "Focus in Medical Anthropology (Society, Culture and Language)", postCode = T.pack "ASFOC2112B", postDescription = T.pack ""})
    , (T.pack "Anthropology Specialist (Society, Culture, and Language) (Arts Program) - ASSPE2112", Post {postName = Specialist, postDepartment = T.pack "Anthropology (Society, Culture, and Language) (Arts Program)", postCode = T.pack "ASSPE2112", postDescription = T.pack ""})
    , (T.pack "Christianity and Culture: Major Program in Religious Education (Arts Program) - ASMAJ1021", Post {postName = Major, postDepartment = T.pack "Christianity and Culture: Religious Education (Arts Program)", postCode = T.pack "ASMAJ1021", postDescription = T.pack ""})
    , (T.pack "Minor in French Language (Arts Program) - ASMIN0120", Post {postName = Minor, postDepartment = T.pack "French Language (Arts Program)", postCode = T.pack "ASMIN0120", postDescription = T.pack ""})
    , (T.pack "Minor Program in Christianity and Education (Arts Program) - ASMIN1014", Post {postName = Minor, postDepartment = T.pack "Christianity and Education (Arts Program)", postCode = T.pack "ASMIN1014", postDescription = T.pack ""})
    , (T.pack "Psychology Research Specialist - Thesis (Science Program) - ASSPE1958)", Post {postName = Specialist, postDepartment = T.pack "Psychology Research - Thesis (Science Program)", postCode = T.pack "ASSPE1958", postDescription = T.pack ""})
    , (T.pack "Cognitive Science Major - Arts (Language and Cognition Stream) (Arts Program) - ASMAJ1445B", Post {postName = Major, postDepartment = T.pack "Cognitive Science - Arts (Language and Cognition Stream) (Arts Program)", postCode = T.pack "ASMAJ1445B", postDescription = T.pack ""})
    , (T.pack "Certificate in Business Fundamentals - ASCER2400", Post {postName = Certificate, postDepartment = T.pack "Business Fundamentals", postCode = T.pack "ASCER2400", postDescription = T.pack ""})
    , (T.pack "Focus in Green Chemistry", Post {postName = Other, postDepartment = T.pack "Focus in Green Chemistry", postCode = T.pack "", postDescription = T.pack ""})
    , (T.pack "Focus in Finance - ASFOC2431B", Post {postName = Other, postDepartment = T.pack "Focus in Finance", postCode = T.pack "ASFOC2431B", postDescription = T.pack ""})
    , (T.pack "Biological Physics Specialist", Post {postName = Specialist, postDepartment = T.pack "Biological Physics", postCode = T.pack "", postDescription = T.pack ""})
    ]
postInfoTests :: Test
postInfoTests = createTest postInfoParser "Post requirements" postInfoInputs

-- functions for running tests in REPL
postTestSuite :: Test
postTestSuite = TestLabel "PostParser tests" $ TestList [postInfoTests]
