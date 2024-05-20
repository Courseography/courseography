module Controllers.Graph (graphResponse, findAndSavePrereqsResponse, graphs) where

import Happstack.Server (ServerPart, Response, toResponse, ok)
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
    liftIO $ generateAndSavePrereqResponse coursesOptions


graphs :: IO Response
graphs = runSqlite databasePath $ do
    graphsList :: [Entity Graph] <- selectList [GraphDynamic ==. False] [Asc GraphTitle]
    return $ createJSONResponse graphsList :: SqlPersistM Response


