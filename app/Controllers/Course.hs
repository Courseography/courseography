module Controllers.Course
    (retrieveCourse, index, courseInfo) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T (Text, unlines)
import qualified Database.CourseQueries as CourseHelpers (getDeptCourses)
import Database.Persist (Entity)
import Database.Persist.Sqlite (SqlPersistM, entityVal, selectList)
import Database.Tables as Tables (Courses, coursesCode)
import Happstack.Server (Response, ServerPart, lookText', toResponse)
import Models.Course (returnCourse)
import Util.Happstack (createJSONResponse)

-- | Takes a course code (e.g. \"CSC108H1\") and sends a JSON representation
-- of the course as a response.
retrieveCourse :: ServerPart Response
retrieveCourse = do
    name <- lookText' "name"
    courseJSON <- liftIO $ returnCourse name
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
