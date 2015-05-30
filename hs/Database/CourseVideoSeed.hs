{-# LANGUAGE OverloadedStrings #-}

module Database.CourseVideoSeed where

import Data.Text (Text)
import Database.Tables hiding (Text)
import Database.Persist.Sqlite (runSqlite, updateWhere, (=.), (==.))
import Config (dbStr)

courseVideos :: [(Text, [Text])]
courseVideos = [
  ("CSC240H1", ["static/videos/csc240.mp4"]),
  ("CSC336H1", ["static/videos/csc336.mp4"]),
  ("CSC436H1", ["static/videos/csc436.mp4"]),
  ("CSC438H1", ["static/videos/csc438.mp4"]),
  ("CSC456H1", ["static/videos/csc456.mp4"]),
  ("CSC463H1", ["static/videos/csc463.mp4"])]

seedVideo (code, videos) = do
    updateWhere [CoursesCode ==. code] [CoursesVideoUrls =. videos]


seedVideos :: IO ()
seedVideos = runSqlite dbStr $ do
    mapM_ seedVideo courseVideos