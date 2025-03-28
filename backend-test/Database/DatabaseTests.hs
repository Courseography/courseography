{-|
Description: Database test suites module.

Module that contains the test suites for all the database functions.

-}

module Database.DatabaseTests
(  databaseTests  ) where

import Test.HUnit (Test (..))
import Database.CourseQueriesTests (courseQueriesTestSuite)

-- Single test encompassing all database test suites
databaseTests :: Test
databaseTests = TestList [courseQueriesTestSuite]
