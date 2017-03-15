{-|
Description: Main module for testing.

Module that acts as interface for testing multiple test suites using cabal.

-}

module Main
(  main  ) where

import qualified System.Exit as Exit
import qualified Text.Parsec as Parsec
import ParserTests.ParserTests
import Test.HUnit ( assertEqual, runTestTT, Test(..), failures )

-- Single test encompassing all test suites
tests :: Test
tests = TestList $ [strTestSuite, reqTestSuite]

main :: IO ()
main = do
    count <- runTestTT tests
    if failures count > 0 then Exit.exitFailure else return ()
