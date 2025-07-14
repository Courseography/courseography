{-|
Description: Database test suites module.

Module that contains the test suites for all the database functions.

-}

module Database.DatabaseTests
(  databaseTests  ) where

import Database.CourseQueriesTests (courseQueriesTestSuite)
import Test.Tasty (TestTree, testGroup)

-- Single test encompassing all database test suites
databaseTests :: TestTree
databaseTests = testGroup "Database" [courseQueriesTestSuite]
