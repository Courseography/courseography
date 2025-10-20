{-|
Description: Program Controller module tests.

Module that contains the tests for the functions in the Program Controller module.
-}

module Controllers.ProgramControllerTests (
    test_programController
) where

import Config (runDb)
import Controllers.Program (index, retrieveProgram)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), decode, withObject, (.:))
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Database.DataType (ProgramType (..))
import Database.Persist.Sqlite (SqlPersistM, insert_)
import Database.Tables (Program (..))
import Happstack.Server (rsBody)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertEqual, testCase)
import TestHelpers (mockGetRequest, clearDatabase, runServerPart, runServerPartWith, withDatabase)

-- | A Program response without timestamps (for comparison purposes)
data ProgramResponseNoTime = ProgramResponseNoTime
    { programCode :: T.Text
    , programDepartment :: T.Text
    , programDescription :: T.Text
    , programName :: T.Text
    , programRequirements :: T.Text
    } deriving (Show, Eq)

instance FromJSON ProgramResponseNoTime where
    parseJSON = withObject "Expected Object for Program" $ \o -> do
        code <- o .: "programCode"
        dept <- o .: "programDepartment"
        desc <- o .: "programDescription"
        name <- o .: "programName"
        reqs <- o .: "programRequirements"
        return $ ProgramResponseNoTime code dept desc name reqs

-- | List of test cases as (label, programs to insert, query params, expected output)
retrieveprogramTestCases :: [(String, [T.Text], T.Text, Maybe ProgramResponseNoTime)]
retrieveprogramTestCases =
    [ ("Valid program code returns JSON"
      , ["ASMAJ1689"]
      , "ASMAJ1689"
      , Just (ProgramResponseNoTime "ASMAJ1689" "test" "test" "Other" "test")
      )
    , ("Invalid program code returns null JSON"
      , []
      , "INVALID123"
      , Nothing
      )
    , ("Empty code parameter returns null"
      , []
      , ""
      , Nothing
      )
    ]

-- | Parse response and extract non-timestamp fields
parseProgramResponse :: String -> Maybe ProgramResponseNoTime
parseProgramResponse jsonStr = decode (BL.pack jsonStr)

-- | Run a test case (case, input, expected output) on the retrieveProgram function.
runRetrieveProgramTest :: String -> [T.Text] -> T.Text -> Maybe ProgramResponseNoTime -> TestTree
runRetrieveProgramTest label programs queryParam expected =
    testCase label $ do
        runDb $ do
            clearDatabase
            insertPrograms programs
        response <- runServerPartWith Controllers.Program.retrieveProgram $ mockGetRequest "/code" [("code" ,T.unpack queryParam)] ""
        let actual = BL.unpack $ rsBody response
        let parsedActual = parseProgramResponse actual
        assertEqual ("Unexpected JSON response body for" ++ label) expected parsedActual

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
runIndexTest label programs expected =
    testCase label $ do
        runDb $ do
            clearDatabase
            insertPrograms programs
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
            insert_ (Program Other "test" code "test" "test" curr curr)

-- | Run all the index test cases
runIndexTests :: [TestTree]
runIndexTests = map (\(label, programs, expected) -> runIndexTest label programs expected) indexTestCases

-- | Test suite for Program Controller Module
test_programController :: TestTree
test_programController = withDatabase "Program Controller tests" (runIndexTests ++ runRetrieveProgramTests)
