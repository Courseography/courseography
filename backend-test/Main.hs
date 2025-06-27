{-|
Description: Main module for testing.

Module that acts as interface for testing multiple test suites using cabal.

-}

module Main where

import Config (databasePath)
import Control.Monad (when)
import Controllers.ControllerTests (controllerTests)
import Data.Text (unpack)
import Database.Database (setupDatabase)
import Database.DatabaseTests (databaseTests)
import RequirementTests.RequirementTests (requirementTests)
import SvgTests.SvgTests (svgTests)
import System.Directory (removeFile)
import System.Environment (setEnv, unsetEnv)
import qualified System.Exit as Exit
import Test.HUnit (Test (..), failures, runTestTT)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
    setEnv "APP_ENV" "test"
    setupDatabase
    defaultMain tests
    path <- databasePath
    removeFile $ unpack path
    unsetEnv "APP_ENV"

tests :: TestTree
tests = testGroup "Tests" [controllerTests, databaseTests]
