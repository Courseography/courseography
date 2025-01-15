module Controllers.Course
    (retrieveCourse, index, courseInfo, depts) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.List (sort, nub, find)
import Data.Maybe (fromMaybe)
import qualified Database.CourseQueries as CourseHelpers (queryCourse, getDeptCourses)
import Database.Persist (Entity)
import Database.Persist.Sqlite (SqlPersistM, selectList, entityVal)
import qualified Data.Text as T
import Database.Tables as Tables (Courses, coursesCode)
import Happstack.Server (lookText', ServerPart, Response, toResponse, rqInputsQuery, Input(..), askRq)
import Util.Happstack (createJSONResponse)

-- Helper function to look for the "name" key and extract its value (Right case)
extractName :: [(String, Input)] -> String
extractName inputs =
    fromMaybe "" $ do
        (_, Input (Right value) _ _) <- find (\(key, _) -> key == "name") inputs
        return (BSL.unpack value)

-- | Takes a course code (e.g. \"CSC108H1\") and sends a JSON representation
-- of the course as a response.
retrieveCourse :: ServerPart Response
retrieveCourse = do
    req <- askRq
    let name = extractName (rqInputsQuery req)
    courseJSON <- liftIO $ CourseHelpers.queryCourse (T.pack name)
    return $ createJSONResponse courseJSON

-- | Builds a list of all course codes in the database.
index :: ServerPart Response
index = do
  response <- liftIO $ runDb $ do
      coursesList :: [Entity Courses] <- selectList [] []
      let codes = map (coursesCode . entityVal) coursesList
      return $ T.unlines codes :: SqlPersistM T.Text
  return $ toResponse response

  -- | Returns all course info for a given department.
courseInfo :: ServerPart Response
courseInfo = do
    dept <- lookText' "dept"
    fmap createJSONResponse (CourseHelpers.getDeptCourses dept)

-- | Return a list of all departments.
depts :: ServerPart Response
depts = do
    deptList <- liftIO $ runDb $ do
        coursesList :: [Entity Courses] <- selectList [] []
        return $ sort . nub $ map g coursesList :: SqlPersistM [String]
    return $ createJSONResponse deptList
    where
        g = take 3 . T.unpack . coursesCode . entityVal