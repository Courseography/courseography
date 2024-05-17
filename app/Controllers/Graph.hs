module Controllers.Graph (graphResponse, findAndSavePrereqsResponse, queryGraphs) where

import Happstack.Server (ServerPart, Response, toResponse, ok)
import MasterTemplate
import Scripts
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import DynamicGraphs.GraphOptions (CourseGraphOptions (..))
import DynamicGraphs.WriteRunDot hiding (findAndSavePrereqsResponse)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import Data.Maybe (fromJust)
import Database.Tables as Tables
import Database.Persist
import Database.Persist.Sqlite
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

queryGraphs :: IO Response
queryGraphs = runSqlite databasePath $ do
    graphs :: [Entity Graph] <- selectList [GraphDynamic ==. False] [Asc GraphTitle]
    return $ createJSONResponse graphs :: SqlPersistM Response


