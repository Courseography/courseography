{-|
    Module      : Database.CourseVideoSeed
    Description : Contains the data and functions for seeding the courseVideos.
-}
module Database.CourseVideoSeed
    (courseVideos, seedVideos) where

import Config (runDb)
import Data.Text (Text)
import Database.Persist.Sqlite (SqlPersistM, updateWhere, (=.), (==.))
import Database.Tables hiding (Text)

-- | Defines the constant list of ordered pairs pertaining to the routes for
-- course videos. The first Text variable in each ordered pair is the course.
-- The second List of Text variable is the routes to the videos.
courseVideos :: [(Text, [Text])]
courseVideos = [
  ("CSC240H1", ["/static/videos/csc240.mp4"]),
  ("CSC336H1", ["/static/videos/csc336.mp4"]),
  ("CSC436H1", ["/static/videos/csc436.mp4"]),
  ("CSC438H1", ["/static/videos/csc438.mp4"]),
  ("CSC456H1", ["/static/videos/csc456.mp4"]),
  ("CSC463H1", ["/static/videos/csc463.mp4"])]

seedVideo :: (Text, [Text]) -> SqlPersistM ()
seedVideo (code, videos) =
    updateWhere [CoursesCode ==. code] [CoursesVideoUrls =. videos]

-- | Sets the video routes of all course rows.
seedVideos :: IO ()
seedVideos = runDb $ mapM_ seedVideo courseVideos
