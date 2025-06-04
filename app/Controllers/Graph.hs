module Controllers.Graph (graphResponse, index, getGraphJSON, graphImageResponse) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe)
import Happstack.Server (Response, ServerPart, look, lookText', ok, toResponse)
import MasterTemplate (header, masterTemplate)
import Scripts (graphScripts)
import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Config (runDb)
import Database.Persist.Sqlite (Entity, SelectOpt (Asc), SqlPersistM, selectList, (==.))
import Database.Tables as Tables (EntityField (GraphDynamic, GraphTitle), Graph, Text)
import Export.GetImages (getActiveGraphImage)
import Models.Graph (getGraph)
import Response.Image (returnImageData)
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


index :: ServerPart Response
index = liftIO $ runDb $ do
    graphsList :: [Entity Graph] <- selectList [GraphDynamic ==. False] [Asc GraphTitle]
    return $ createJSONResponse graphsList :: SqlPersistM Response


-- | Looks up a graph using its title then gets the Shape, Text and Path elements
-- for rendering graph (returned as JSON).
getGraphJSON :: ServerPart Response
getGraphJSON = do
    graphName <- lookText' "graphName"
    response <- liftIO $ getGraph graphName
    return $ createJSONResponse $ fromMaybe (object ["texts" .= ([] :: [Text]),
                                                    "shapes" .= ([] :: [Text]),
                                                    "paths" .= ([] :: [Text])]) response


-- | Returns an image of the graph requested by the user, given graphInfo stored in local storage.
graphImageResponse :: ServerPart Response
graphImageResponse = do
    graphInfo <- look "JsonLocalStorageObj"
    (svgFilename, imageFilename) <- liftIO $ getActiveGraphImage graphInfo
    liftIO $ returnImageData svgFilename imageFilename
