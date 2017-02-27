{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

-}
-- Example.hs  --  Examples from HUnit user's guide
--
-- For more examples, check out the tests directory.  It contains unit tests
-- for HUnit.

module ParserTests.ParserTests
( runStrTests,
  runReqTests
) where

import Test.HUnit
import Database.Requirement
import WebParsing.ReqParser
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Control.Monad.Identity (Identity)

-- create function that takes in a parser and string,
-- applies parser to string and creates assertion as in test 2?

strTest1 :: Test
strTest1 = TestCase (assertEqual "for (fcesParser \"5.0 FCEs\")," (Right "5.0") (Parsec.parse fcesParser "" "5.0 FCEs"))

strTest2 :: Test
strTest2 = TestCase (assertEqual "for (fcesParser \"1 FCEs\")," (Right "1") (Parsec.parse fcesParser "" "1 FCEs"))

strTest3 :: Test
strTest3 = TestCase (assertEqual "for (fcesParser \"2.5 asdf\")," (Right "2.5") (Parsec.parse fcesParser "" "5.0 asdf"))

strTest4 :: Test
strTest4 = TestCase (assertEqual "for (gradeParser \"A+\")," (Right "5.0") (Parsec.parse fcesParser "" "5.0 FCEs"))

strTest5 :: Test
strTest5 = TestCase (assertEqual "for (gradeParser \"80\")," (Right "80") (Parsec.parse gradeParser "" "80"))

strTest6 :: Test
strTest6 = TestCase (assertEqual "for (gradeParser \"6.0\")," (Right "6.0") (Parsec.parse gradeParser "" "6.0"))

strTest7 :: Test
strTest7 = TestCase (assertEqual "for (gradeParser \"A-\")," (Right "A") (Parsec.parse gradeParser "" "-A"))


strTest8 :: Test
strTest8 = TestCase(let courseReq = Parsec.parse courseParser "" " csc148h1 "
                    in assertEqual "for (gradeParser \" csc148h1 \")," (Right $ J"csc148h1") courseReq)

strTest9 :: Test
strTest9 = TestCase(let courseReq = Parsec.parse orParser "" " csc148h1 "
                    in assertEqual "for (orParser \" csc148h1 \")," (Right $ OR[J"csc148h1"]) courseReq)

strTest10 :: Test
strTest10 = TestCase(let courseReq = Parsec.parse andParser "" " csc148h1,csc165h1 "
                    in assertEqual "for (andParser \" csc148h1,csc165h1 \")," (Right $ AND[J"csc148h1",J"csc165h1"]) courseReq)

strTest11 :: Test
strTest11 = TestCase(let courseReq = Parsec.parse andParser "" " csc108h1/csc148h1,csc165h1 "
                    in assertEqual "for (andParser \" csc108h1,csc148h1,csc165h1 \")," (Right $ AND[OR[J"csc108h1",J"csc148h1"],J"csc165h1"]) courseReq)

-- Function to facilitate test case creation given a string, Req tuple
createTest :: (String, Req) -> Parser Req -> Test
createTest (input, expected) parser = TestCase (let courseReq =  Parsec.parse parser "" input
    in assertEqual ("for (" ++ input ++ "),") (Right expected) courseReq)

-- coBefTests
coBefInputs :: [(String, Req)]
coBefInputs = [(" minimum of 60% in csc148h1", GRADE "60" (J "csc148h1")),("a minimum of A in csc165h1", GRADE "A" (J "csc165h1")), (" a minimum of (B-) in csc165h1", GRADE "B-" (J "csc165h1"))]
coBefTests :: Test
coBefTests = TestList $ (map (\parserToTest -> parserToTest coBefParser) (map createTest coBefInputs))

-- coAftTests
coAftInputs :: [(String, Req)]
coAftInputs = [("csc148 with a minimum grade of 60% ", GRADE "60" (J "csc148h1")),(" csc165h1 (A)", GRADE "A" (J "csc165h1")), (" csc108h1 (B-)", GRADE "B-" (J "csc165h1"))]
coAftTests :: Test
coAftTests = TestList $ (map (\parserToTest -> parserToTest coAftParser) (map createTest coAftInputs))

-- singleParserTests
singleInputs :: [(String, Req)]
singleInputs = [("csc108h1", J"csc108h1"), ("csc165Y1", J"csc165Y1"), ("CSC207H1", J"CSC207H1")]
singleTests :: Test
singleTests = TestList $ (map (\parserToTest -> parserToTest singleParser) (map createTest singleInputs))

-- orTests
orInputs :: [(String, Req)]
orInputs = [("csc108h1", J"csc108h1"),("csc108h1 or csc148h1", OR[J"csc108h1",J"csc148h1"]), ("csc108h1 or (csc148h1)", OR[J"csc108h1",J"csc148h1"]), ("csc104h1/csc108h1,csc165h1", OR[RAW "csc104h1/csc108h1",J"csc165h1"])]
orTests :: Test
orTests = TestList $ (map (\parserToTest -> parserToTest orParser) (map createTest orInputs))

-- andTests
andInputs :: [(String, Req)]
andInputs = [("csc108h1", J"csc108h1"),("csc108h1 and csc148h1", AND[J"csc108h1",J"csc148h1"]), ("csc104h1/csc108h1/csc165h1", AND[OR[J"csc104h1",J"csc108h1"],J"csc148h1"])]
andTests :: Test
andTests = TestList $ (map (\parserToTest -> parserToTest andParser) (map createTest andInputs))

-- fromTests
fromInputs :: [(String, Req)]
fromInputs = [(" 5.0 fces from csc148h1 ", FROM "5.0" (J "csc148h1")),(" 2 FCEs from either csc148h1/csc165h1 ", FROM "2" (OR[J"csc148h1",J"csc165h1"])), (" 5 fces from ", FROM "5" (RAW ""))]
fromTests :: Test
fromTests = TestList $ (map (\parserToTest -> parserToTest fromParser) (map createTest fromInputs))


-- functions for running tests in REPL
strTests :: Test
strTests = TestList [strTest1, strTest2, strTest3, strTest4, strTest5, strTest6, strTest7, strTest8, strTest9, strTest1, strTest1]

reqTests :: Test
reqTests = TestList [coBefTests, coAftTests, singleTests, orTests, andTests, fromTests]

runStrTests :: IO Counts
runStrTests = runTestTT strTests
runReqTests :: IO Counts
runReqTests = runTestTT reqTests
