module Models.Course
    (buildCourse,
    returnCourse,
    prereqsForCourse,
    getDeptCourses,
    insertCourse) where

import Config (runDb)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T (Text, append, filter, snoc, take, toUpper)
import Database.Persist.Class (selectKeysList)
import Database.Persist.Sqlite (Entity, PersistText, SqlPersistM, entityVal, get, insert_, rawSql,
                                selectFirst, selectList, (<-.), (==.))
import Database.Tables hiding (breadth, distribution)
import Models.Meeting (buildMeetTimes, meetingQuery)

-- | Queries the database for all information about @course@,
-- constructs and returns a Course value.
returnCourse :: T.Text -> IO (Maybe Course)
returnCourse lowerStr = runDb $ do
    let courseStr = T.toUpper lowerStr
    -- TODO: require the client to pass the full course code
    let fullCodes = [courseStr, T.append courseStr "H1", T.append courseStr "Y1"]
    sqlCourse :: (Maybe (Entity Courses)) <- selectFirst [CoursesCode <-. fullCodes] []
    case sqlCourse of
      Nothing -> return Nothing
      Just course -> do
        meetings <- meetingQuery fullCodes
        Just <$> buildCourse meetings
                                (entityVal course)

-- | Queries the database for the breadth description
getDescriptionB :: Maybe (Key Breadth) -> SqlPersistM (Maybe T.Text)
getDescriptionB Nothing = return Nothing
getDescriptionB (Just key) = do
    maybeBreadth <- get key
    return $ fmap breadthDescription maybeBreadth

-- | Queries the database for the distribution description
getDescriptionD :: Maybe (Key Distribution) -> SqlPersistM (Maybe T.Text)
getDescriptionD Nothing = return Nothing
getDescriptionD (Just key) = do
    maybeDistribution <- get key
    return $ fmap distributionDescription maybeDistribution

-- | Builds a Course structure from a tuple from the Courses table.
-- Some fields still need to be added in.
buildCourse :: [MeetTime'] -> Courses -> SqlPersistM Course
buildCourse allMeetings course = do
    cBreadth <- getDescriptionB (coursesBreadth course)
    cDistribution <- getDescriptionD (coursesDistribution course)
    return $ Course cBreadth
           -- TODO: Remove the filter and allow double-quotes
           (fmap (T.filter (/='\"')) (coursesDescription course))
           (fmap (T.filter (/='\"')) (coursesTitle course))
           (coursesPrereqString course)
           (Just allMeetings)
           (coursesCode course)
           (coursesExclusions course)
           cDistribution
           (coursesCoreqs course)
           (coursesVideoUrls course)

-- | Retrieves the prerequisites for a course (code) as a string.
-- Also retrieves the actual course code in the database in case
-- the one the user inputs doesn't match it exactly
prereqsForCourse :: T.Text -> IO (Either String (T.Text, T.Text))
prereqsForCourse courseCode = runDb $ do
    let upperCaseCourseCode = T.toUpper courseCode
    course <- selectFirst [CoursesCode <-. [upperCaseCourseCode, upperCaseCourseCode `T.append` "H1", upperCaseCourseCode `T.append` "Y1"]] []
    case course of
        Nothing -> return (Left "Course not found")
        Just courseEntity ->
            return (Right
                     (coursesCode $ entityVal courseEntity,
                      fromMaybe "" $ coursesPrereqString $ entityVal courseEntity)
                    ) :: SqlPersistM (Either String (T.Text, T.Text))

getDeptCourses :: MonadIO m => T.Text -> m [Course]
getDeptCourses dept = liftIO $ runDb $ do
        courses :: [Entity Courses] <- rawSql "SELECT ?? FROM courses WHERE code LIKE ?" [PersistText $ T.snoc dept '%']
        let deptCourses = map entityVal courses
        meetings :: [Entity Meeting] <- selectList [MeetingCode <-. map coursesCode deptCourses] []
        mapM (processCourse meetings) deptCourses
    where
        processCourse allMeetings course = do
            let courseMeetings = filter (\m -> meetingCode (entityVal m) == coursesCode course) allMeetings
            allTimes <- mapM buildMeetTimes courseMeetings
            buildCourse allTimes course

--contains' :: PersistEntity m => T.Text -> SqlPersistM m
--contains field query = Filter field (Left $ T.concat ["%", query, "%"]) (BackendSpecificFilter "LIKE")

-- Get Key of correspondig record in Distribution column
getDistributionKey :: T.Text -> SqlPersistM (Maybe (Key Distribution))
getDistributionKey description_ = do
    keyListDistribution :: [Key Distribution] <- selectKeysList [ DistributionDescription ==. description_ ] []
    -- option: keyListDistribution :: [DistributionId] <- selectKeysList [ DistributionDescription `contains'` description] []
    return $ case keyListDistribution of
        [] -> Nothing
        (x:_) -> Just x

getBreadthKey :: T.Text -> SqlPersistM (Maybe (Key Breadth))
getBreadthKey description_ = do
    keyListBreadth :: [Key Breadth] <- selectKeysList [ BreadthDescription ==. description_ ] []
    -- option: selectKeysList [ BreadthDescription `contains'` description] []
    return $ case keyListBreadth of
        [] -> Nothing
        (x:_) -> Just x

-- | Inserts course into the Courses table.
insertCourse :: (Courses, T.Text, T.Text) -> SqlPersistM ()
insertCourse (course, breadth, distribution) = do
    maybeCourse <- selectFirst [CoursesCode ==. coursesCode course] []
    breadthKey <- getBreadthKey breadth
    distributionKey <- getDistributionKey distribution
    case maybeCourse of
        Nothing -> insert_ $ course {coursesBreadth = breadthKey,
                                     coursesDistribution = distributionKey}
        Just _ -> return ()
