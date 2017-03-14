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


-- functions for running tests in REPL
strTestSuite :: Test
strTestSuite = TestList [fcesTests, gradeTests]

reqTestSuite :: Test
reqTestSuite = TestList [coBefTests, coAftTests, singleTests, orTests, andTests, fromTests]
