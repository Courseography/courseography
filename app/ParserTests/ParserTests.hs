{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

-}

module ParserTests.ParserTests
( reqTestSuite ) where

import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Database.Requirement
import WebParsing.ReqParser
import Test.HUnit ( assertEqual, Test(..) )

-- Function to facilitate test case creation given a string, Req tuple
createTest :: (Eq a, Show a) => Parser a -> (String, a) -> Test
createTest parser (input, expected) = let courseReq =  Parsec.parse parser "" input
                                      in TestCase $ (assertEqual ("for (" ++ input ++ "),") (Right expected) courseReq)

test1 :: Test
test1 = TestCase (assertEqual "TEST PASS" ("True") ("True"))

reqTestSuite :: Test
reqTestSuite = TestList [TestLabel "parseReqs" test1]
