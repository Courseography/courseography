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
queryCourse course = runSqlite (T.pack ("database/" ++ T.unpack dbStr)) $ do


        sqlCourse :: [Entity Courses] <- selectList [CoursesCode ==. (T.pack course)] []

        sqlLecturesFall :: [Entity Lectures]  <- selectList [LecturesCode  ==. (T.pack course),
                                                             LecturesSession ==. "F"] []

        sqlLecturesSpring :: [Entity Lectures]  <- selectList [LecturesCode  ==. (T.pack course), 
                                                               LecturesSession ==. "S"] []

        sqlTutorialsFall :: [Entity Tutorials] <- selectList [TutorialsCode ==. (T.pack course), 
                                                              TutorialsSession ==. "F"] []
                                                              
        sqlTutorialsSpring :: [Entity Tutorials] <- selectList [TutorialsCode ==. (T.pack course), 
                                                                TutorialsSession ==. "S"] []
        
        let x = entityVal $ head sqlCourse

        let fallLectures = map entityVal sqlLecturesFall
        let springLectures = map entityVal sqlLecturesSpring
        let fallTutorials = map entityVal sqlTutorialsFall
        let springTutorials = map entityVal sqlTutorialsSpring
        
        let fallLecturesExtracted = map buildLecture fallLectures
        let springLecturesExtracted = map buildLecture springLectures
        let fallTutorialsExtracted = map buildTutorial fallTutorials
        let springTutorialsExtracted = map buildTutorial springTutorials

        let fallSession   = JsonParser.Session fallLecturesExtracted fallTutorialsExtracted
        let springSession = JsonParser.Session springLecturesExtracted springTutorialsExtracted

        -- Some fields still need to be added in.
        let courseJSON = Course (coursesBreadth x)
                                (coursesDescription x)
                                (coursesTitle x)
                                 Nothing               --prereqString
                                (Just fallSession)
                                (Just springSession)
                                (coursesCode x)        --name
                                (coursesExclusions x)  --exclusions
                                 Nothing               -- manualTutorialEnrolment
                                (coursesDistribution x)
                                 Nothing               -- prereqs

        return $ toResponse $ createJSONResponse $ encodeJSON $ Aeson.toJSON courseJSON

-- | Builds a Lecture structure from a tuple from the Lectures table.
buildLecture :: Lectures -> Lecture
buildLecture entity = Lecture (lecturesExtra entity)
                              (lecturesSection entity)
                              (lecturesCapacity entity)
                              (lecturesTime_str entity)
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
