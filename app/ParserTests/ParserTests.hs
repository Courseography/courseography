{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

-}
module ParserTests.ParserTests
(  strTestSuite,
   reqTestSuite  ) where

import qualified System.Exit as Exit
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Database.Requirement
import WebParsing.ReqParser
import Test.HUnit ( assertEqual, runTestTT, Test(..), failures )

-- Function to facilitate test case creation given a string, Req tuple
createTest :: (Eq a, Show a) => Parser a -> (String, a) -> Test
createTest parser (input, expected) = let courseReq =  Parsec.parse parser "" input
                                      in TestCase $ (assertEqual ("for (" ++ input ++ "),") (Right expected) courseReq)

reqParserTest1 :: Test
reqParserTest1 = let courseReq = Parsec.parse courseParser "" " csc148h1 "
                 in TestCase $ (assertEqual "for (gradeParser \" csc148h1 \")," (Right $ J "csc148h1") courseReq)

reqParserTest2 :: Test
reqParserTest2 = let courseReq = Parsec.parse orParser "" " csc148h1 "
                 in TestCase $ (assertEqual "for (orParser \" csc148h1 \")," (Right $ OR[J "csc148h1"]) courseReq)

reqParserTest3 :: Test
reqParserTest3 = let courseReq = Parsec.parse andParser "" " csc148h1,csc165h1 "
                 in TestCase $ (assertEqual "for (andParser \" csc148h1,csc165h1 \")," (Right $ AND[J "csc148h1",J "csc165h1"]) courseReq)

reqParserTest4 :: Test
reqParserTest4 = let courseReq = Parsec.parse andParser "" " csc108h1/csc148h1,csc165h1 "
                 in TestCase $ (assertEqual "for (andParser \" csc108h1,csc148h1,csc165h1 \")," (Right $ AND[OR[J "csc108h1",J "csc148h1"],J "csc165h1"]) courseReq)

-- fcesTests
fcesInputs:: [(String, String)]
fcesInputs = [("5.0 FCEs", "5.0"),("1 FCEs", "1"),("5.0 asdf", "2.5"),("5.0 FCEs", "5.0"),("80", "80"),("6.0", "6.0"),("-A", "A")]
fcesTests :: Test
fcesTests = TestLabel "fcesParser" $ TestList $ (map (createTest fcesParser) fcesInputs)

-- gradeTests
gradeInputs:: [(String, String)]
gradeInputs = [("5.0 FCEs", "5.0"),("1 FCEs", "1"),("5.0 asdf", "2.5"),("5.0 FCEs", "5.0"),("80", "80"),("6.0", "6.0"),("-A", "A")]
gradeTests :: Test
gradeTests = TestLabel "fcesParser" $ TestList $ (map (createTest gradeParser) gradeInputs)

-- coBefTests
coBefInputs :: [(String, Req)]
coBefInputs = [(" minimum of 60% in csc148h1", GRADE "60" (J "csc148h1")),("a minimum of A in csc165h1", GRADE "A" (J "csc165h1")), (" a minimum of (B-) in csc165h1", GRADE "B-" (J "csc165h1"))]
coBefTests :: Test
coBefTests = TestLabel "coBefParser" $ TestList $ (map (createTest coBefParser) coBefInputs)

-- coAftTests
coAftInputs :: [(String, Req)]
coAftInputs = [("csc148 with a minimum grade of 60% ", GRADE "60" (J "csc148h1")),(" csc165h1 (A)", GRADE "A" (J "csc165h1")), (" csc108h1 (B-)", GRADE "B-" (J "csc165h1"))]
coAftTests :: Test
coAftTests = TestLabel "coAftParser" $ TestList $ (map (createTest coAftParser) coAftInputs)

-- singleParserTests
singleInputs :: [(String, Req)]
singleInputs = [("csc108h1", J "csc108h1"), ("csc165Y1", J "csc165Y1"), ("CSC207H1", J "CSC207H1")]
singleTests :: Test
singleTests = TestLabel "singleParser" $ TestList $ (map (createTest singleParser) singleInputs)

-- orTests
orInputs :: [(String, Req)]
orInputs = [("csc108h1", J "csc108h1"),("csc108h1 or csc148h1", OR[J "csc108h1",J "csc148h1"]), ("csc108h1 or (csc148h1)", OR[J "csc108h1",J "csc148h1"]), ("csc104h1/csc108h1,csc165h1", OR[RAW "csc104h1/csc108h1",J "csc165h1"])]
orTests :: Test
orTests = TestLabel "orParser" $ TestList $ (map (createTest orParser) orInputs)

-- andTests
andInputs :: [(String, Req)]
andInputs = [("csc108h1", J "csc108h1"),("csc108h1 and csc148h1", AND[J "csc108h1",J "csc148h1"]), ("csc104h1/csc108h1/csc165h1", AND[OR[J "csc104h1",J "csc108h1"],J "csc148h1"])]
andTests :: Test
andTests = TestLabel "andParser" $ TestList $ (map (createTest andParser) andInputs)

-- fromTests
fromInputs :: [(String, Req)]
fromInputs = [(" 5.0 fces from csc148h1 ", FROM "5.0" (J "csc148h1")),(" 2 FCEs from either csc148h1/csc165h1 ", FROM "2" (OR[J "csc148h1",J "csc165h1"])), (" 5 fces from ", FROM "5" (RAW ""))]
fromTests :: Test
fromTests = TestLabel "fromParser" $ TestList $ (map (createTest fromParser) fromInputs)

-- categoryParser Tests
reqInputs :: [(String, Req)]
reqInputs = [("CSC120H1/CSC148H1", OR [J "CSC120H1", J "CSC148H1"]), ("CSC120H1, CSC121H1, CSC148H1", AND [J "CSC120H1", J "CSC121H1", J "CSC148H1"]), 
("CSC148H1/(CSC108H1/CSC120H1, MAT137Y1/MAT157Y1)", OR [J "CSC148H1", AND [OR [J "CSC108H1", J "CSC120H1"], OR [J "MAT137Y1", J "MAT157Y1"]]]), 
("CSC436H1/(CSC336H1 (75%))", OR [J "CSC436H1", GRADE "75" J "CSC336H1"]), ("60% or higher in CSC148H1/CSC150H1", GRADE "60" OR [J "CSC148H1", J "CSC150H1"]), 
("60% or higher in CSC148H1/CSC150H1, 60% or higher in CSC165H1/CSC240H1", AND [GRADE "60" OR [J "CSC148H1", J "CSC150H1"], GRADE "60" OR [J "CSC165H1", J "CSC240H1"]]), 
("CSC240H1 or an A- in CSC236H1", OR [J "CSC240H1", GRADE "A-" J "CSC236H1"]), ("CSC258H1; CSC209H1/proficiency in C", AND [J "CSC258H1", OR [J "CSC209H1", RAW "proficiency in C"]]), 
("STA247H1/STA255H1/STA257H1/PSY201H1/ECO227Y1, (MAT135H1, MAT136H1)/MAT137Y1/MAT157Y1", AND [OR [J "STA247H1", J "STA255H1", J "STA257H1", J "PSY201H1", J "ECO227Y1"], OR [AND [J "MAT135H1", J "MAT136H1"], J "MAT137Y1", J "MAT157Y1"]]), 
("(MAT136H1 with a minimum mark of 77)/(MAT137Y1 with a minimum mark of 73)/(MAT157Y1 with a minimum mark of 67)/MAT235Y1/MAT237Y1/MAT257Y1, MAT221H1/MAT223H1/MAT240H1; STA247H1/STA255H1/STA257H1", AND [AND [OR [GRADE "77" J "MAT136H1", GRADE "73" J "MAT137Y1", GRADE "67" J "MAT157Y1", J "MAT235Y1", J "MAT237Y1", J "MAT257Y1"], OR [J "MAT221H1", J "MAT223H1", J "MAT240H1"]], OR [J "STA247H1", J "STA255H1", J "STA257H1"]]), 
("60% or higher in CSC148H1/CSC150H1; STA247H1/STA255H1/STA257H1; (MAT135H1, MAT136H1)", AND [GRADE "60" OR [J "CSC148H1", J "CSC150H1"], OR [J "STA247H1", J "STA255H1", J "STA257H1"], AND [J "MAT135H1", J "MAT136H1"]]), 
("CSC209H1/(CSC207H1, proficiency in C or C++); MAT221H1/MAT223H1/MAT240H1, (MAT136H1 with a minimum mark of 77)/(MAT137Y1 with a minimum mark of 73)/(MAT157Y1 with a minimum mark of 67)/MAT235Y1/MAT237Y1/MAT257Y1", AND [OR [J "CSC209H1", AND [J "CSC207H1", RAW "proficiency in C or C++"]], AND [OR [J "MAT221H1", J "MAT223H1", J "MAT240H1"], OR [GRADE "77" J "MAT136H1", GRADE "73" J "MAT137Y1", GRADE "67" J "MAT157Y1", J "MAT235Y1", J "MAT237Y1", J "MAT257Y1"]]]), 
("CSC148H1/CSC150H1; MAT133Y1(70%)/(MAT135H1, MAT136H1)", AND [OR [J "CSC148H1", J "CSC150H1"], OR [GRADE "70" J "MAT133Y1", AND [J "MAT135H1", J "MAT136H1"]]]), 
("MAT221H1/MAT223H1/MAT240H1 is strongly recommended", OR [J "MAT221H1", J "MAT223H1", J "MAT240H1"]), ("CSC209H1/proficiency in C or C++;  Prerequisite for Engineering students only: ECE345H1 or ECE352H1", AND [OR [J "CSC209H1", RAW "proficiency in C or C++"], RAW "Prerequisite for Engineering students only: ECE345H1 or ECE352H1"]), 
("CSC209H1/proficiency in C or C++ or Java", OR [J "CSC209H1", RAW "proficiency in C or C++", RAW "Java"]), ("(CSC336H1 (75%))/equivalent mathematical background; MAT237Y1/MAT257Y1", AND [OR [GRADE "75" J "CSC336H1", RAW "equivalent mathematical background"], OR [J "MAT237Y1", J "MAT257Y1"]])]
reqTests :: Test
reqTests = TestLabel "categoryParser" $ TestList $ (map (createTest categoryParser) reqInputs)


-- functions for running tests in REPL
strTestSuite :: Test
strTestSuite = TestList [fcesTests, gradeTests]

reqTestSuite :: Test
reqTestSuite = TestList [coBefTests, coAftTests, singleTests, orTests, andTests, fromTests, reqTests]
