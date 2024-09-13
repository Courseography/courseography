module Controllers.Graph (graphResponse, index, getGraphJSON, graphImageResponse) where

import Happstack.Server (ServerPart, Response, toResponse, ok)
import MasterTemplate (masterTemplate, header)
import Scripts (graphScripts)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text as T (Text)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))

import Database.Tables as Tables
    ( EntityField(GraphTitle, GraphDynamic), Text, Graph )
import Database.CourseQueries (getGraph)
import Database.Persist.Sqlite
    ( Entity,
      SelectOpt(Asc),
      (==.),
      selectList,
      runSqlite,
      SqlPersistM )
import Config (databasePath)
import Util.Happstack (createJSONResponse)
import Export.GetImages (getActiveGraphImage)
import Response.Image (returnImageData)

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


index :: ServerPart Response
index = liftIO (runSqlite databasePath $ do
    graphsList :: [Entity Graph] <- selectList [GraphDynamic ==. False] [Asc GraphTitle]
    return $ createJSONResponse graphsList :: SqlPersistM Response)


-- | Looks up a graph using its title then gets the Shape, Text and Path elements
-- for rendering graph (returned as JSON).
getGraphJSON :: T.Text -> IO Response
getGraphJSON graphName = getGraph graphName >>= withDefault
    where
        withDefault (Just response) = return response
        withDefault Nothing = return $
            createJSONResponse $
            object ["texts" .= ([] :: [Text]),
                    "shapes" .= ([] :: [Text]),
                    "paths" .= ([] :: [Text])]


-- | Returns an image of the graph requested by the user, given graphInfo stored in local storage.
graphImageResponse :: String -> ServerPart Response
graphImageResponse graphInfo = do
    (svgFilename, imageFilename) <- liftIO $ getActiveGraphImage graphInfo
    liftIO $ returnImageData svgFilename imageFilename