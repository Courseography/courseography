module Controllers.Graph (graphResponse, findAndSavePrereqsResponse, index) where

import Happstack.Server (ServerPart, Response, toResponse, ok, badRequest)
import MasterTemplate (masterTemplate, header)
import Scripts (graphScripts)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import DynamicGraphs.GraphOptions (CourseGraphOptions (..))
import DynamicGraphs.WriteRunDot (getBody, generateAndSavePrereqResponse)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import Database.Tables as Tables
import Database.Persist.Sqlite
    ( Entity,
      SelectOpt(Asc),
      (==.),
      selectList,
      runSqlite,
      SqlPersistM )
import Config (databasePath)
import Util.Happstack (createJSONResponse)
import Database.CourseQueries as CourseHelper (getAllCourseCodes)
import qualified Data.Text.Lazy as TL

graphResponse :: ServerPart Response
graphResponse =
   ok $ toResponse $
    masterTemplate "Courseography - Graph"
                []
                (do
                    header "graph"
                    H.div ! A.id "container" $ ""
                )
                graphScripts

findAndSavePrereqsResponse :: ServerPart Response
findAndSavePrereqsResponse = do
    body <- getBody
    let coursesOptions :: CourseGraphOptions = fromJust $ decode body
    let selectedCourses = courses coursesOptions
    allCourses <- liftIO CourseHelper.getAllCourseCodes
    let isValid = any ((`elem` allCourses) . TL.unpack) selectedCourses
    if isValid
        then 
            liftIO $ generateAndSavePrereqResponse coursesOptions
        else do
            badRequest $ toResponse ("Invalid course selected" :: String)
            
index :: ServerPart Response
index = liftIO (runSqlite databasePath $ do
    graphsList :: [Entity Graph] <- selectList [GraphDynamic ==. False] [Asc GraphTitle]
    return $ createJSONResponse graphsList :: SqlPersistM Response)


