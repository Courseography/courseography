module RequirementTests.CourseControllerTests
( courseContTestSuite ) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BL
import Test.HUnit
import Happstack.Server
import Happstack.Server.Internal.Monads
import Happstack.Server.Internal.Types
import Happstack.Server.Types
import Happstack.Server.Monads
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Except (runExceptT)
import Control.Exception (catch, IOException)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Logger (NoLoggingT)
import Control.Concurrent.MVar
import Database.Persist.Sql (SqlPersistM)
import Database.Tables
import Database.Persist.Sqlite (insert_)
import Database.Database(setupDatabase)
import Controllers.Course (index)
import System.Environment (setEnv, unsetEnv, getEnv)
import Happstack.Server.SimpleHTTP

-- | Create a minimal mock request
minimalRequest :: Request
minimalRequest = Request
        { rqSecure          = False             
        , rqMethod          = GET               
        , rqPaths           = []                
        , rqUri             = "/"               
        , rqQuery           = ""                
        , rqInputsQuery     = []               
        , rqCookies         = []                
        , rqVersion         = HttpVersion 1 1   
        , rqPeer            = ("127.0.0.1", 0)  
        }


-- | Helper function to run ServerPart Response
runServerPart :: ServerPart Response -> IO Response
runServerPart sp = do
    runWebT $ runServerPartT sp minimalRequest

-- | Helper function to create test database
createDatabase :: [T.Text] -> SqlPersistM ()
createDatabase courses = do
    liftIO setupDatabase
    mapM_ (\courseCode -> insert_ (Courses courseCode Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing [])) courses


-- | Test case for the `index` function when there is one course in the database.
testIndexOneCourse :: Test
testIndexOneCourse = TestCase $ do
    runDb $ createDatabase ["CSC137"]
    let expectedResponse = ["CSC137"]
    resp <- runServerPart Controllers.Course.index
    let body = BL.unpack $ rsBody resp
    assertEqual "Should return one course" expectedResponse body


-- TODO: Write Tests for empty database and for multiple (5) Courses


-- | Create the test suite
courseContTestSuite :: IO Test
courseContTestSuite = do
    setEnv "APP_ENV" "test"
    let tests = TestLabel "Course Controller tests" $ TestList [testIndexOneCourse]
    return tests
