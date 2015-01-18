{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleInstances #-}

module Main where
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import Control.Monad    (msum)
import Network.HTTP.Conduit (withManager)
import Happstack.Server
import GridResponse
import GraphResponse
import AboutResponse
import qualified Data.Text.Lazy as L
import JsonParser
import Tables
import qualified Data.Aeson as Aeson
import Control.Monad.IO.Class  (liftIO)

import Database.Persist
import Database.Persist.Sqlite

import Filesystem.Path.CurrentOS
import System.Directory
import qualified Facebook as FB

graph :: String
graph = "graph"

grid :: String
grid = "grid"

about :: String
about = "about"

static :: String
static = "static"

app :: FB.Credentials
app = FB.Credentials "localhost" "442286309258193" "INSERT_SECRET"

url :: FB.RedirectUrl
url = "http://localhost:8000/test"

perms :: [FB.Permission]
perms = ["user_birthday"]

fb :: String
fb = "fb"

course :: String
course = "course"

test :: String
test = "test"

code :: String
code = "graph-fb"

main :: IO ()
main = do
    print url
    cwd <- getCurrentDirectory

    let staticDir = encodeString $ parent $ decodeString cwd
    simpleHTTP nullConf $
      msum [ dir grid $ gridResponse,
             dir graph $ graphResponse,
             dir code $ seeOther fbAuth1Url $ toResponse test,
             dir test $ look "code" >>= getEmail,
             dir about $ aboutResponse,
             dir static $ serveDirectory EnableBrowsing [] staticDir,
             dir course $ path (\s -> liftIO $ queryCourse s)
           ]

getEmail :: String -> ServerPart Response
getEmail cdf = liftIO $ retrieveFBData cdf

fbAuth1Url :: String
fbAuth1Url = "https://www.facebook.com/dialog/oauth?client_id=442286309258193&redirect_uri=http://localhost:8000/test&scope=user_birthday"

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

-- | The arguments passed to the API. The first argument is the key of the query data ('code')
-- and the second argument is the code that was retrieved in the first authorization step.
args :: String -> FB.Argument
args code = ("code", BS.pack code)

-- | Retrieves the user's email.s
retrieveFBData :: String -> IO Response
retrieveFBData code = withManager $ \manager -> FB.runFacebookT app manager $ do
        token <- FB.getUserAccessTokenStep2 url [args code]
        u <- FB.getUser "me" [] (Just token)
        liftIO $ insertIdIntoDb (FB.userId u)
        return $ toResponse (FB.userEmail u)

insertIdIntoDb :: FB.Id -> IO ()
insertIdIntoDb id_ = runSqlite fbdbStr $ do
                       runMigration migrateAll
                       insert_ $ FacebookTest (show id_) "Test String"