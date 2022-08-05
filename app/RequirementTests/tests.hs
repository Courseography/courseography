{-|
Description: Main module for testing.

Module that acts as interface for testing multiple test suites using cabal.

-}

module Main
(  main  ) where

import Control.Monad
import RequirementTests.ModifierTests (modifierTestSuite)
import RequirementTests.PostParserTests (postTestSuite)
import RequirementTests.ReqParserTests (reqTestSuite)
import qualified System.Exit as Exit
import Test.HUnit (Test (..), failures, runTestTT)

-- Single test encompassing all test suites
tests :: Test
tests = TestList [reqTestSuite, postTestSuite, modifierTestSuite]

main :: IO ()
main = do
    count <- runTestTT tests
    Control.Monad.when (failures count > 0) Exit.exitFailure
