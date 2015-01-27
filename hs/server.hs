{-# LANGUAGE OverloadedStrings,
             ScopedTypeVariables,
             FlexibleInstances,
             FlexibleContexts #-}

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
import JsonParser
import qualified Data.Conduit.List as CL
import Control.Monad.Trans.Resource
import Tables
import qualified Data.Aeson as Aeson
import Control.Monad.IO.Class  (liftIO)

import Database.Persist
import Data.Conduit (($$))
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

-- In order to access information as the Courseography application, the secret needs
-- to be declared in the third string below that has the place holder 'INSERT_SECRET'.
-- The secret should NEVER be committed to GitHub.
-- The secret can be found here: https://developers.facebook.com/apps/442286309258193/dashboard/
-- Should the secret be committed to GitHub, it needs to be reset immediately. If you find
-- yourself in this pickle, please contact someone who can do this.
app :: FB.Credentials
app = FB.Credentials "localhost" "442286309258193" "INSERT_SECRET"

url :: FB.RedirectUrl
url = "http://localhost:8000/test"

url2 :: FB.RedirectUrl
url2 = "http://localhost:8000/test-post"

perms :: [FB.Permission]
perms = []

course :: String
course = "course"

test :: String
test = "test"

testPost :: String
testPost = "test-post"

code :: String
code = "graph-fb"

postFB :: String
postFB = "post-fb"

main :: IO ()
main = do
    cwd <- getCurrentDirectory
    let staticDir = encodeString $ parent $ decodeString cwd
    simpleHTTP nullConf $
      msum [ dir grid $ gridResponse,
             dir graph $ graphResponse,
             dir code $ seeOther fbAuth1Url $ toResponse test,
             dir test $ look "code" >>= getEmail,
             dir testPost $ look "code" >>= postToFacebook,
             dir about $ aboutResponse,
             dir static $ serveDirectory EnableBrowsing [] staticDir,
             dir course $ path (\s -> liftIO $ queryCourse s),
             dir postFB $ seeOther fbAuth1UrlPost $ toResponse test
           ]

postToFacebook :: String -> ServerPart Response
postToFacebook code = (liftIO $ performPost code) >> graphResponse

args2 :: FB.Argument
args2 = ("message", "Test post please ignore")

performPost :: String -> IO Response
performPost code = withManager $ \manager -> FB.runFacebookT app manager $ do
        postToFB code =<< getToken url2 code
        return $ toResponse postFB

-- | Gets a user access token.
getToken :: (MonadResource m, MonadBaseControl IO m) => FB.RedirectUrl -> String -> FB.FacebookT FB.Auth m FB.UserAccessToken
getToken url code = FB.getUserAccessTokenStep2 url [args code]

-- | Posts a message to Facebook.
postToFB :: (MonadResource m, MonadBaseControl IO m) => String -> FB.UserAccessToken -> FB.FacebookT FB.Auth m FB.Id
postToFB code token = FB.postObject "me/feed" [args2] token

-- | Gets a users Facebook email.
getEmail :: String -> ServerPart Response
getEmail code = liftIO $ retrieveFBData code

fbAuth1Url :: String
fbAuth1Url = "https://www.facebook.com/dialog/oauth?client_id=442286309258193&redirect_uri=http://localhost:8000/test&scope=user_birthday"

fbAuth1UrlPost :: String
fbAuth1UrlPost = "https://www.facebook.com/dialog/oauth?client_id=442286309258193&redirect_uri=http://localhost:8000/test-post"

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

-- | Retrieves the user's email.
retrieveFBData :: String -> IO Response
retrieveFBData code = withManager $ \manager -> FB.runFacebookT app manager $ do
        token <- getToken url code
        user <- FB.getUser "me" [] token
        liftIO $ insertIdIntoDb (FB.userId user)
        return $ toResponse (FB.userEmail user)

insertIdIntoDb :: FB.Id -> IO ()
insertIdIntoDb id_ = runSqlite fbdbStr $ do
                       runMigration migrateAll
                       insert_ $ FacebookTest (show id_) "Test String"
                       liftIO $ print "Inserted..."
                       let sql = "SELECT * FROM facebook_test"
                       rawQuery sql [] $$ CL.mapM_ (liftIO . print)
