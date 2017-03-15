{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

-}
-- Example.hs  --  Examples from HUnit user's guide
--
-- For more examples, check out the tests directory.  It contains unit tests
-- for HUnit.

module ParserTests.ParserTests
(  reqTestSuite  ) where

import Test.HUnit
import Database.Requirement
import WebParsing.ReqParser

-- create function that takes in a parser and string,
-- applies parser to string and creates assertion as in test 2?

test1 :: Test
test1 = TestCase (assertEqual "for (parseReqs \"csc108h1 or csc148h1\")," (OR [J"csc108h1",J"csc148h1"]) (parseReqs "csc108h1 or csc148h1"))


reqTestSuite :: Test
reqTestSuite = TestList [TestLabel "parseReqs" test1]
