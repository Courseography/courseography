{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}

module Main where
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Control.Monad    (msum)
import Happstack.Server
import GridResponse
import GraphResponse
import AboutResponse
import JsonParser
import Tables
import qualified Data.Aeson as Aeson
import Control.Monad.IO.Class  (liftIO)

import Database.Persist
import Database.Persist.Sqlite

graph :: String
graph = "graph"

grid :: String
grid = "grid"

about :: String
about = "about"

static :: String
static = "static"

staticDir :: String
--staticDir = "C:\\Users\\David\\Documents\\courseography"
staticDir = "/home/cynic/4/courseography"

course :: String
course = "course"

main :: IO ()
main = simpleHTTP nullConf $
  msum [ dir grid $ gridResponse,
         dir graph $ graphResponse,
         dir about $ aboutResponse,
         dir static $ serveDirectory EnableBrowsing [] staticDir,
         dir course $ path (\s -> liftIO $ queryCourse s)
       ]

-- | Queries the database for all information about `course`, constructs a JSON object 
-- | representing the course and returns the appropriate JSON response.
queryCourse :: String -> IO Response
queryCourse courseStr = runSqlite (T.pack ("database/" ++ T.unpack dbStr)) $ do


        sqlCourse :: [Entity Courses] <- selectList [CoursesCode ==. (T.pack courseStr)] []

        sqlLecturesFall :: [Entity Lectures]  <- selectList [LecturesCode  ==. (T.pack courseStr),
                                                             LecturesSession ==. "F"] []

        sqlLecturesSpring :: [Entity Lectures]  <- selectList [LecturesCode  ==. (T.pack courseStr), 
                                                               LecturesSession ==. "S"] []

        sqlLecturesYear :: [Entity Lectures]  <- selectList [LecturesCode  ==. (T.pack courseStr), 
                                                               LecturesSession ==. "Y"] []

        sqlTutorialsFall :: [Entity Tutorials] <- selectList [TutorialsCode ==. (T.pack courseStr), 
                                                              TutorialsSession ==. "F"] []

        sqlTutorialsSpring :: [Entity Tutorials] <- selectList [TutorialsCode ==. (T.pack courseStr), 
                                                                TutorialsSession ==. "S"] []

        sqlTutorialsYear :: [Entity Tutorials] <- selectList [TutorialsCode ==. (T.pack courseStr), 
                                                                TutorialsSession ==. "Y"] []

        let course = entityVal $ head sqlCourse

        let fallLectures = map entityVal sqlLecturesFall
        let springLectures = map entityVal sqlLecturesSpring
        let yearLectures = map entityVal sqlLecturesYear

        let fallTutorials = map entityVal sqlTutorialsFall
        let springTutorials = map entityVal sqlTutorialsSpring
        let yearTutorials = map entityVal sqlTutorialsYear
        
        let fallLecturesExtracted = map buildLecture fallLectures
        let springLecturesExtracted = map buildLecture springLectures
        let yearLecturesExtracted = map buildLecture yearLectures

        let fallTutorialsExtracted = map buildTutorial fallTutorials
        let springTutorialsExtracted = map buildTutorial springTutorials
        let yearTutorialsExtracted = map buildTutorial yearTutorials

        let fallSession   = JsonParser.Session fallLecturesExtracted fallTutorialsExtracted
        let springSession = JsonParser.Session springLecturesExtracted springTutorialsExtracted
        let yearSession = JsonParser.Session yearLecturesExtracted yearTutorialsExtracted

        let courseJSON = buildCourse (Just fallSession) (Just springSession) (Just yearSession) course

        return $ toResponse $ createJSONResponse $ encodeJSON $ Aeson.toJSON courseJSON

-- | Builds a Course structure from a tuple from the Courses table.
-- Some fields still need to be added in.
buildCourse :: Maybe Session -> Maybe Session -> Maybe Session -> Courses -> Course 
buildCourse fallSession springSession yearSession course = Course (coursesBreadth course)
                                                                  (coursesDescription course)
                                                                  (coursesTitle course)
                                                                   Nothing               --prereqString
                                                                  (fallSession)
                                                                  (springSession)
                                                                  (yearSession)
                                                                  (coursesCode course)        --name
                                                                  (coursesExclusions course)  --exclusions
                                                                   Nothing               -- manualTutorialEnrolment
                                                                  (coursesDistribution course)
                                                                   Nothing               -- prereqs

-- | Builds a Lecture structure from a tuple from the Lectures table.
buildLecture :: Lectures -> Lecture
buildLecture entity = Lecture (lecturesExtra entity)
                              (lecturesSection entity)
                              (lecturesCapacity entity)
                              (lecturesTimeStr entity)
                              (map timeField (lecturesTimes entity))
                              (lecturesInstructor entity)
                              (Just (lecturesEnrolled entity))
                              (Just (lecturesWaitlist entity))

-- | Builds a Tutorial structure from a tuple from the Tutorials table.
buildTutorial :: Tutorials -> Tutorial
buildTutorial entity = Tutorial (map timeField (tutorialsTimes entity))
                                (tutorialsTimeStr entity)

-- | Encodes an Aeson Value into a ByteString.
encodeJSON :: Aeson.Value -> BSL.ByteString
encodeJSON json = BSL.pack $ filter (\c -> c /= '\\') $ BSL.unpack $ Aeson.encode $ json

-- | Creates a JSON response.
createJSONResponse :: BSL.ByteString -> Response
createJSONResponse jsonStr = toResponseBS (BS.pack "application/json") $ jsonStr
