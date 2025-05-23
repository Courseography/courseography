module Controllers.Course
    (retrieveCourse, index, courseInfo, depts) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import Data.List (nub, sort)
import qualified Data.Text as T (Text, unlines, unpack)
import qualified Database.CourseQueries as CourseHelpers (getDeptCourses, queryCourse)
import Database.Persist (Entity)
import Database.Persist.Sqlite (SqlPersistM, entityVal, selectList)
import Database.Tables as Tables (Courses, coursesCode)
import Happstack.Server (Response, ServerPart, lookText', toResponse)
import Util.Happstack (createJSONResponse)

-- | Takes a course code (e.g. \"CSC108H1\") and sends a JSON representation
-- of the course as a response.
retrieveCourse :: ServerPart Response
retrieveCourse = do
    name <- lookText' "name"
    courseJSON <- liftIO $ CourseHelpers.queryCourse name
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
