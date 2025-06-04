module Models.Graph 
    (getGraph,
    getGraphJSON) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, toJSON, (.=))
import Data.List (partition)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text)
import Database.DataType (ShapeType (BoolNode, Hybrid, Node))
import Database.Persist.Sqlite (Entity, PersistEntity, PersistValue (PersistInt64),
                                entityKey, entityVal, keyToValues, selectFirst,
                                selectList, (<-.), (==.))
import Database.Tables as Tables
import Happstack.Server (Response, ServerPart, lookText')
import Svg.Builder (buildEllipses, buildPath, buildRect, intersectsWithShape)
import Util.Happstack (createJSONResponse)

getGraph :: T.Text -> IO (Maybe Value)
getGraph graphName = runDb $ do
    graphEnt :: (Maybe (Entity Graph)) <- selectFirst [GraphTitle ==. graphName] []
    case graphEnt of
        Nothing -> return Nothing
        Just graph -> do
            let gId = entityKey graph
            sqlTexts    :: [Entity Text] <- selectList [TextGraph ==. gId] []
            sqlRects    :: [Entity Shape] <- selectList
                                                 [ShapeType_ <-. [Node, Hybrid],
                                                  ShapeGraph ==. gId] []
            sqlEllipses :: [Entity Shape] <- selectList
                                                 [ShapeType_ ==. BoolNode,
                                                  ShapeGraph ==. gId] []
            sqlPaths    :: [Entity Path] <- selectList [PathGraph ==. gId] []

            let
                keyAsInt :: PersistEntity a => Entity a -> Integer
                keyAsInt = fromIntegral . (\(PersistInt64 x) -> x) . head . keyToValues . entityKey

                graphtexts          = map entityVal sqlTexts
                rects          = zipWith (buildRect graphtexts)
                                         (map entityVal sqlRects)
                                         (map keyAsInt sqlRects)
                ellipses       = zipWith (buildEllipses graphtexts)
                                         (map entityVal sqlEllipses)
                                         (map keyAsInt sqlEllipses)
                graphpaths     = zipWith (buildPath rects ellipses)
                                         (map entityVal sqlPaths)
                                         (map keyAsInt sqlPaths)
                (regions, _)   = partition pathIsRegion graphpaths
                regionTexts    = filter (not .
                                         intersectsWithShape (rects ++ ellipses))
                                        graphtexts

                response = object [
                        ("texts", toJSON $ graphtexts ++ regionTexts),
                        ("shapes", toJSON $ rects ++ ellipses),
                        ("paths", toJSON $ graphpaths ++ regions),
                        ("width", toJSON $ graphWidth $ entityVal graph),
                        ("height", toJSON $ graphHeight $ entityVal graph)
                    ]

            return (Just response)

-- | Looks up a graph using its title then gets the Shape, Text and Path elements
-- for rendering graph (returned as JSON).
getGraphJSON :: ServerPart Response
getGraphJSON = do
    graphName <- lookText' "graphName"
    response <- liftIO $ getGraph graphName
    return $ createJSONResponse $ fromMaybe (object ["texts" .= ([] :: [Text]),
                                                    "shapes" .= ([] :: [Text]),
                                                    "paths" .= ([] :: [Text])]) response
