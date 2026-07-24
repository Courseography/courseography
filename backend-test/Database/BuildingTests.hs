-- |
-- Description: Building module tests.
--
-- Module that contains the tests for the functions in the Building module.
module Database.BuildingTests (
    test_buildings,
) where

import Config (runDb)
import Database.Persist.Sqlite (Filter, selectList)
import Database.Tables (Building)
import Models.Building (buildingsCSV, getBuildingsFromCSV, parseBuildings)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (clearDatabase, withDatabase)

-- | Count the number of buildings currently in the database.
countBuildings :: IO Int
countBuildings = do
    buildings <- runDb $ selectList ([] :: [Filter Building]) []
    return $ length buildings

-- | Count the number of building rows in the CSV that parseBuildings reads.
expectedBuildingCount :: IO Int
expectedBuildingCount = do
    csvPath <- buildingsCSV
    buildings <- getBuildingsFromCSV csvPath
    return $ length buildings

-- | Run test on parseBuildings to check for a non-zero number of buildings
testParseBuildingsInserts :: TestTree
testParseBuildingsInserts =
    testCase "parseBuildings inserts buildings from the CSV" $ do
        runDb clearDatabase
        parseBuildings
        count <- countBuildings
        expected <- expectedBuildingCount
        assertEqual "Expected parseBuildings to insert every building row from the CSV" expected count

-- | Run test on parseBuildings to check that calling multiple times does not duplicate entries in the database
testParseBuildingsIdempotent :: TestTree
testParseBuildingsIdempotent =
    testCase "parseBuildings is called multiple times and does not duplicate entries" $ do
        runDb clearDatabase
        parseBuildings
        countAfterFirst <- countBuildings
        parseBuildings
        countAfterSecond <- countBuildings
        expected <- expectedBuildingCount
        assertEqual "Expected parseBuildings to insert every building row from the CSV" expected countAfterFirst
        assertEqual
            "Expected building count to be the same after multiple calls"
            countAfterFirst
            countAfterSecond

-- | Test suite for Building module
test_buildings :: TestTree
test_buildings =
    withDatabase "Building tests" [testParseBuildingsInserts, testParseBuildingsIdempotent]
