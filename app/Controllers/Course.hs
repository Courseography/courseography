module Controllers.Course
    (retrieveCourse, index, courseInfo, depts) where

import Config (databasePath)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T (Text, unlines, unpack)
import Data.List (sort, nub)
import Database.Persist (Entity)
import Database.Persist.Sqlite (SqlPersistM, runSqlite, selectList, entityVal)
import Database.Tables as Tables (Courses, coursesCode)
import Happstack.Server(lookText', ServerPart, Response, toResponse)
import Util.Happstack (createJSONResponse)
import qualified Database.CourseQueries as CourseHelpers (queryCourse, getDeptCourses)

-- | Takes a course code (e.g. \"CSC108H1\") and sends a JSON representation
-- of the course as a response.
retrieveCourse :: ServerPart Response
retrieveCourse = lookText' "name" >>= liftIO . CourseHelpers.queryCourse

-- | Builds a list of all course codes in the database.
index :: ServerPart Response
index = do
  response <- liftIO $ runSqlite databasePath $ do
      coursesList :: [Entity Courses] <- selectList [] []
      let codes = map (coursesCode . entityVal) coursesList
      return $ T.unlines codes :: SqlPersistM T.Text
  return $ toResponse response

  -- | Returns all course info for a given department.
courseInfo :: ServerPart Response
courseInfo = fmap createJSONResponse (lookText' "dept" >>= CourseHelpers.getDeptCourses)

-- | Return a list of all departments.
depts :: ServerPart Response
depts = do
    deptList <- liftIO $ runSqlite databasePath $ do
        coursesList :: [Entity Courses] <- selectList [] []
        return $ sort . nub $ map g coursesList :: SqlPersistM [String]
    return $ createJSONResponse deptList
    where
        g = take 3 . T.unpack . coursesCode . entityVal
