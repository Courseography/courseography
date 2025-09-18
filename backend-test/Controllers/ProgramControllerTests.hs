module Controllers.ProgramControllerTests (
    test_programController
) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import Controllers.Program (index)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.DataType (PostType (..))
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables (Post (..))
import Happstack.Server (rsBody)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (clearDatabase, runServerPart, withDatabase)

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
        insertProgram code = do
            curr <- liftIO getCurrentTime
            insert_ (Post Other "test" code "test" "test" curr curr)

-- | Run all the index test cases
runIndexTests :: [TestTree]
runIndexTests = map (\(label, programs, expected) -> runIndexTest label programs expected) indexTestCases

-- | Test suite for Program Controller Module
test_programController :: TestTree
test_programController = withDatabase "Program Controller tests" runIndexTests
