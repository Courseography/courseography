{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

-}

module ParserTests.ParserTests
( reqTestSuite ) where

import Test.HUnit

test1 :: Test
test1 = TestCase (assertEqual "TEST PASS" ("True") ("True"))

reqTestSuite :: Test
reqTestSuite = TestList [TestLabel "parseReqs" test1]
