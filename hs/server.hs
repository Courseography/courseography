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

data Dummy = Dummy {dummField :: T.Text, dumm2Field :: T.Text}

main :: IO ()
main = simpleHTTP nullConf $
  msum [ dir grid $ gridResponse,
         dir graph $ graphResponse,
         dir about $ aboutResponse,
         dir static $ serveDirectory EnableBrowsing [] staticDir,
         dir course $ path (\s -> liftIO $ queryCourse s)
       ]

queryCourse :: String -> IO Response
queryCourse course = runSqlite (T.pack ("database/" ++ T.unpack dbStr)) $ do
        sqlCourse    :: [Entity Courses] <- selectList [CoursesCode ==. (T.pack course)] []
        let x = entityVal $ head sqlCourse
        sqlLectures  :: [Entity Lectures] <- selectList [LecturesCode ==. (T.pack course)] []
        sqlTutorials :: [Entity Tutorials] <- selectList [TutorialsCode ==. (T.pack course)] []
        let y = (map entityVal) sqlLectures
        let z = (map entityVal) sqlTutorials
        let y1 = map extractLecture y
        let z1 = map extractTutorial z
        let session = JsonParser.Session y1 z1
        let d = Course (coursesBreadth x)
        	           (coursesDescription x)
        	           (coursesTitle x)
        	            Nothing --prereqString
        	           (Just session) --f
        	           (Just session) --s
        	           (coursesCode x)  --name
        	           (coursesExclusions x) --exclusions
        	            Nothing -- man tut
        	           (coursesDistribution x)
        	            Nothing -- prereqs               :: Maybe [Text]

        return $ toResponse $ formatJsonResonse $ encodeJSON (Aeson.toJSON d)
        --sqlTutorials :: [Entity Tutorials] <- selectList [TutorialsCode ==. "CSC108H1"] []
        --return $ formatJsonResonse $
        --          (BSL.pack $
        --           removeQuotationMarks $
        --           (filter (\c -> c /= '\\') $ 
        --           	BSL.unpack $
        --            Aeson.encode $ 
        --            (toJsonText $ 
        --             entityVal $ 
        --             head sqlCourse)))


extractLecture :: Lectures -> Lecture
extractLecture ent = Lecture (lecturesExtra ent)
                             (lecturesSection ent)
                             (lecturesCapacity ent)
                             (lecturesTime_str ent)
                             (map timeField (lecturesTimes ent))
                             (lecturesInstructor ent)
                             (Just (lecturesEnrolled ent))
                             (Just (lecturesWaitlist ent))

extractTutorial :: (Tutorials) -> Tutorial
extractTutorial ent = Tutorial (map timeField (tutorialsTimes ent))
                               (tutorialsTimeStr ent)

encodeJSON :: Aeson.Value -> BSL.ByteString
encodeJSON x = BSL.pack $
                   --removeQuotationMarks $
                   filter (\c -> c /= '\\') $ 
                   	BSL.unpack $
                    Aeson.encode $ x

formatJsonResonse :: BSL.ByteString -> Response
formatJsonResonse x = toResponseBS (BS.pack "application/json") $ x

removeQuotationMarks :: String -> String
removeQuotationMarks x = (reverse $ tail $ reverse $ tail $ x)

