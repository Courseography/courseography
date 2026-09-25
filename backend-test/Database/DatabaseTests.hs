-- |
-- Description: Database module tests
--
-- Module that contains the tests for the functions in the Database module.
module Database.DatabaseTests (
    test_database,
) where

import Config (runDb)
import Data.List (sort)
import qualified Data.Text as T
import Database.Database (populateStaticInfo)
import Database.Persist.Sqlite (Entity, entityVal, selectList)
import Database.Tables (
    Breadth (..),
    Distribution (..),
 )
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (clearDatabase, withDatabase)

-- | Run test on populateStaticInfo to check that distribution and breadth tables are set up
testPopulateStaticInfo :: TestTree
testPopulateStaticInfo =
    testCase "populateStaticInfo inserts tables into the database" $ do
        runDb clearDatabase
        populateStaticInfo

        distributions <- runDb $ selectList [] [] :: IO [Entity Distribution]
        breadths <- runDb $ selectList [] [] :: IO [Entity Breadth]

        assertEqual
            "Expected populateStaticInfo to insert distribution entries"
            (sort $ map (distributionDescription . entityVal) distributions)
            ( sort
                [ T.pack "Humanities"
                , T.pack "Social Science"
                , T.pack "Science"
                ]
            )
        assertEqual
            "Expected populateStaticInfo to insert breadth entries"
            (sort $ map (breadthDescription . entityVal) breadths)
            ( sort
                [ T.pack "Creative and Cultural Representations (1)"
                , T.pack "Thought, Belief, and Behaviour (2)"
                , T.pack "Society and its Institutions (3)"
                , T.pack "Living Things and Their Environment (4)"
                , T.pack "The Physical and Mathematical Universes (5)"
                , T.pack "No Breadth"
                ]
            )

-- | Test suite for Database module
test_database :: TestTree
test_database =
    withDatabase "Database tests" [testPopulateStaticInfo]
