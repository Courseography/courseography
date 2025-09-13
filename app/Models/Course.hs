module Models.Course
    (buildCourse,
    returnCourse) where

import Config (runDb)
import qualified Data.Text as T (Text, append, filter, take, toUpper)
import Database.Persist.Sqlite (Entity, SqlPersistM, entityKey, entityVal, get, selectFirst,
                                selectList, (<-.), (==.))
import Database.Tables as Tables
import Meeting (meetingQuery)

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
