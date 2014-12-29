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
        let d = Course "breadth"
        	            (coursesTitle x)
        	            (coursesDescription x)
        	            Nothing
        	            Nothing
        	            Nothing
        	            "name"
        	            Nothing
        	            Nothing
        	            "(coursesDistribution x)"
        	            Nothing
             --distribution          :: !Text,
             --prereqs               :: Maybe [Text]
        return $ toResponse $ formatJsonResonse $ encodeJSON (Aeson.toJSON d)
        --sqlLectures  :: [Entity Lectures] <- selectList [LecturesCode ==. "CSC108H1"] []
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

encodeJSON :: Aeson.Value -> BSL.ByteString
encodeJSON x = BSL.pack $
                   removeQuotationMarks $
                   filter (\c -> c /= '\\') $ 
                   	BSL.unpack $
                    Aeson.encode $ x

formatJsonResonse :: BSL.ByteString -> Response
formatJsonResonse x = toResponseBS (BS.pack "application/json") $ x

removeQuotationMarks :: String -> String
removeQuotationMarks x = reverse $ tail $ reverse $ tail $ x