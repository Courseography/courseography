{-|
Description: Program Controller module tests.

Module that contains the tests for the functions in the Program Controller module.
-}

module Controllers.ProgramControllerTests (
    test_programController
) where

import Config (runDb)
import Controllers.Program (index, retrieveProgram)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Time (UTCTime)
import Database.DataType (PostType (..))
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables (Post (..))
import Happstack.Server (rsBody)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (clearDatabase, runServerPart, runServerPartWithProgramQuery, withDatabase)

-- | Arbitrary timestamp for tests
testTimestamp :: UTCTime
testTimestamp = read "2025-10-01 17:47:27.910498 UTC"
 
-- | List of test cases as (label, programs to insert, query params, expected output)
retrieveprogramTestCases :: [(String, [T.Text], T.Text, String)]
retrieveprogramTestCases =
    [ ("Valid program code returns JSON"
      , ["ASMAJ1689"]
      , "ASMAJ1689"
      , "{\"postCode\":\"ASMAJ1689\",\"postCreated\":\"2025-10-01T17:47:27.910498Z\",\"postDepartment\":\"test\",\"postDescription\":\"test\",\"postModified\":\"2025-10-01T17:47:27.910498Z\",\"postName\":\"Other\",\"postRequirements\":\"test\"}"
      )
    , ("Invalid program code returns null JSON"
      , []
      , "INVALID123"
      , "null"
      )
    , ("Empty code parameter returns null"
      , []
      , ""
      , "null"
      )
    ]

-- | Run a test case (case, input, expected output) on the retrieveProgram function.
runRetrieveProgramTest :: String -> [T.Text] -> T.Text -> String -> TestTree
runRetrieveProgramTest label posts queryParam expected =
    testCase label $ do
        runDb $ do
            clearDatabase
            insertPrograms posts

        response <- runServerPartWithProgramQuery Controllers.Program.retrieveProgram (T.unpack queryParam)
        let actual = BL.unpack $ rsBody response
        assertEqual ("Unexpected body for " ++ label) expected actual

-- | Run all the retrieveProgram test cases
runRetrieveProgramTests :: [TestTree]
runRetrieveProgramTests = map (\(label, programs, params, expected) -> runRetrieveProgramTest label programs params expected) retrieveprogramTestCases

-- | List of test cases as (label, input programs, expected output)
indexTestCases :: [(String, [T.Text], String)]
indexTestCases =
    [ ("Empty database", [], "")
    , ("One program", ["ASMAJ1689"], "ASMAJ1689\n")
    , ("Multiple programs", ["ASMAJ1689", "ASSPE1376", "ASMAJ0506", "ASMIN1165", "ASMIN2289"],
       "ASMAJ0506\nASMAJ1689\nASMIN1165\nASMIN2289\nASSPE1376\n")
    ]

-- | Run a test case (case, input, expected output) on the index function.
runIndexTest :: String -> [T.Text] -> String -> TestTree
runIndexTest label posts expected =
    testCase label $ do
        runDb $ do
            clearDatabase
            insertPrograms posts
        response <- runServerPart Controllers.Program.index
        let actual = BL.unpack $ rsBody response
        assertEqual ("Unexpected response body for " ++ label) expected actual

-- | Helper function to insert programs into the database
insertPrograms :: [T.Text] -> SqlPersistM ()
insertPrograms = mapM_ insertProgram
    where
        insertProgram :: T.Text -> SqlPersistM ()
        insertProgram code =
            insert_ (Post Other "test" code "test" "test" testTimestamp testTimestamp)

-- | Run all the index test cases
runIndexTests :: [TestTree]
runIndexTests = map (\(label, programs, expected) -> runIndexTest label programs expected) indexTestCases

-- | Test suite for Program Controller Module
test_programController :: TestTree
test_programController = withDatabase "Program Controller tests" (runIndexTests ++ runRetrieveProgramTests)
