{-|
Description: Main module for testing.

Module that acts as interface for testing multiple test suites using cabal.

-}

module Main
(  main  ) where

import Control.Monad
import RequirementTests.ModifierTests (modifierTestSuite)
import RequirementTests.ParserTests (reqTestSuite)
import qualified System.Exit as Exit
import System.Process as Process
import Test.HUnit (Test (..), failures, runTestTT)

parseZWSSuite :: String
parseZWSSuite = "app/RequirementTests/ParseZeroWidthSpaces.sh"

-- Single test encompassing all test suites
tests :: Test
tests = TestList [reqTestSuite, modifierTestSuite]

main :: IO ()
main = do
    count <- runTestTT tests
    Control.Monad.when (failures count > 0) Exit.exitFailure

    exitCode <- Process.system parseZWSSuite
    Exit.exitWith exitCode
