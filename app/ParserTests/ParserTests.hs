{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

-}
-- Example.hs  --  Examples from HUnit user's guide
--
-- For more examples, check out the tests directory.  It contains unit tests
-- for HUnit.

module ParserTests.ParserTests
( runPracTests,
  runOrTests,
  runAndTests
) where

import Test.HUnit
import Database.Requirement
import WebParsing.ReqParser
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Control.Monad.Identity (Identity)

-- create function that takes in a parser and string,
-- applies parser to string and creates assertion as in test 2?

pracTest1 :: Test
pracTest1 = TestCase (assertEqual "for (parseReqs \"csc108h1 or csc148h1\")," (OR [J"csc108h1",J"csc148h1"]) (parseReqs "csc108h1 or csc148h1"))

pracTest2 :: Test
pracTest2 = TestCase(let courseReq = Parsec.parse courseParser "" " csc148h1 "
                    in assertEqual "for (courseParser \" csc148h1 \")," (Right $ J"csc148h1") courseReq)

pracTest3 :: Test
pracTest3 = TestCase(let courseReq = Parsec.parse orParser "" " csc148h1 "
                    in assertEqual "for (orParser \" csc148h1 \")," (Right $ OR[J"csc148h1"]) courseReq)

pracTest4 :: Test
pracTest4 = TestCase(let courseReq = Parsec.parse andParser "" " csc148h1,csc165h1 "
                    in assertEqual "for (andParser \" csc148h1,csc165h1 \")," (Right $ AND[J"csc148h1",J"csc165h1"]) courseReq)

pracTest5 :: Test
pracTest5 = TestCase(let courseReq = Parsec.parse andParser "" " csc108h1/csc148h1,csc165h1 "
                    in assertEqual "for (andParser \" csc108h1,csc148h1,csc165h1 \")," (Right $ AND[OR[J"csc108h1",J"csc148h1"],J"csc165h1"]) courseReq)

createTest :: (String, Req) -> Parser Req -> Test
createTest (input, expected) parser = TestCase (let courseReq =  Parsec.parse parser "" input
    in assertEqual ("for (" ++ input ++ "),") (Right expected) courseReq)

-- orTests
orInputs :: [(String, Req)]
orInputs = [("csc108h1", J"csc108h1"),("csc108h1 or csc148h1", OR[J"csc108h1",J"csc148h1"]), ("csc104h1/csc108h1,csc165h1", AND[OR[J"csc104h1",J"csc108h1"],J"csc165h1"])]
orTests :: Test
orTests = TestList $ (map (\parserToTest -> parserToTest orParser) (map createTest orInputs))

-- andTests
andInputs :: [(String, Req)]
andInputs = [("csc108h1", J"csc108h1"),("csc108h1 and csc148h1", AND[J"csc108h1",J"csc148h1"]), ("csc104h1/csc108h1/csc165h1", AND[OR[J"csc104h1",J"csc108h1"],J"csc148h1"])]
andTests :: Test
andTests = TestList $ (map (\parserToTest -> parserToTest andParser) (map createTest andInputs))

-- functions for running tests in REPL
tests :: Test
tests = TestList [TestLabel "practice test 1" pracTest1, TestLabel "practice test 2" pracTest2, TestLabel "practice test 3" pracTest3]

-- expecting error on practice test 1
runPracTests :: IO Counts
runPracTests = runTestTT tests
-- expecting
runOrTests :: IO Counts
runOrTests = runTestTT orTests
-- expecting 
runAndTests :: IO Counts
runAndTests = runTestTT andTests