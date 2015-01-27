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

import Filesystem.Path.CurrentOS
import System.Directory

import CssGen

graph :: String
graph = "graph"

grid :: String
grid = "grid"

about :: String
about = "about"

static :: String
static = "static"

course :: String
course = "course"

main :: IO ()
main = do
    generateCSS
    cwd <- getCurrentDirectory
    let staticDir = encodeString $ parent $ decodeString cwd
    contents <- readFile "../README.md"
    simpleHTTP nullConf $
      msum [ dir grid $ gridResponse,
             dir graph $ graphResponse,
             dir about $ aboutResponse contents,
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

        let fallSession   = buildSession sqlLecturesFall sqlTutorialsFall
        let springSession = buildSession sqlLecturesSpring sqlTutorialsSpring
        let yearSession = buildSession sqlLecturesYear sqlTutorialsYear

        let courseJSON = buildCourse fallSession springSession yearSession course

        return $ toResponse $ createJSONResponse $ encodeJSON $ Aeson.toJSON courseJSON

-- | Builds a Course structure from a tuple from the Courses table.
-- Some fields still need to be added in.
buildCourse :: Maybe Session -> Maybe Session -> Maybe Session -> Courses -> Course 
buildCourse fallSession springSession yearSession course = Course (coursesBreadth course)
                                                                  (coursesDescription course)
                                                                  (coursesTitle course)
                                                                  Nothing --prereqString
                                                                  fallSession
                                                                  springSession
                                                                  yearSession
                                                                  (coursesCode course)
                                                                  (coursesExclusions course)
                                                                  (coursesManualTutorialEnrolment course)               -- manualTutorialEnrolment
                                                                  (coursesDistribution course)
                                                                  Nothing -- prereqs

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
buildTutorial entity = Tutorial (tutorialsSection entity)
                                (map timeField (tutorialsTimes entity))
                                (tutorialsTimeStr entity)

-- | Encodes an Aeson Value into a ByteString.
encodeJSON :: Aeson.Value -> BSL.ByteString
encodeJSON json = BSL.filter (\c -> c /= '\\') $ Aeson.encode json

-- | Builds a Session structure from a list of tuples from the Lectures table, and a list of tuples from the Tutorials table.
buildSession :: [Entity Lectures] -> [Entity Tutorials] -> Maybe Tables.Session
buildSession lectures tutorials = Just $ Tables.Session (map buildLecture (map entityVal lectures))
                                                        (map buildTutorial (map entityVal tutorials))

-- | Creates a JSON response.
createJSONResponse :: BSL.ByteString -> Response
createJSONResponse jsonStr = toResponseBS (BS.pack "application/json") jsonStr
