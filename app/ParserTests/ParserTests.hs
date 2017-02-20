{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

-}
-- Example.hs  --  Examples from HUnit user's guide
--
-- For more examples, check out the tests directory.  It contains unit tests
-- for HUnit.

module ParserTest.ParserTests where

import Test.HUnit
import Database.Requirement
import WebParsing.ReqParser
import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Control.Monad.Identity (Identity)

-- create function that takes in a parser and string,
-- applies parser to string and creates assertion as in test 2?

test1 :: Test
test1 = TestCase (assertEqual "for (parseReqs \"csc108h1 or csc148h1\"),"
                OR [J"csc108h1",J"csc148h1"] (parseReqs "csc108h1 or csc148h1"))



test2 :: Test
test2 = TestCase(do courseReq <- courseParser " csc148h1 "
                    assertEqual "for (courseParser \" csc148h1 \"),"
                    J "csc148h1" courseReq)

test3 :: Test
test3 = TestCase(do courseReq <- courseParser " csc148h1 "
                    assertEqual "for (courseParser \" csc148h1 \"),"
                    J "csc148h1" courseReq)

test100 :: Test
test2 = TestCase (do courseReq <-  singleParser " csc148h1 "
                     assertEqual "for (singleParser \" csc148h1 \","
                     J "csc148h1" courseReq

tests :: Test
tests = TestList [TestLabel "parseReqs" test1, TestLabel "testsingleParser" test2]
