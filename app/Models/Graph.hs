module Models.Graph
    (getGraph,
    saveGraphJSON) where

import Config (runDb)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, decode, object, toJSON)
import Data.List (partition)
import qualified Data.Text as T (Text)
import Database.DataType (ShapeType (BoolNode, Hybrid, Node))
import Database.Persist.Sqlite (Entity, PersistEntity, PersistValue (PersistInt64), SqlPersistM,
                                entityKey, entityVal, insert, insertMany_, keyToValues, selectFirst,
                                selectList, (<-.), (==.))
import Database.Tables hiding (paths, shapes, texts)
import Happstack.Server (Response, ServerPart, lookBS, lookText', toResponse)
import Svg.Builder (buildEllipses, buildPath, buildRect, intersectsWithShape)

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

-- | Inserts SVG graph data into Texts, Shapes, and Paths tables
saveGraphJSON :: ServerPart Response
saveGraphJSON = do
    jsonStr <- lookBS "jsonData"
    nameStr <- lookText' "nameData"
    let jsonObj = decode jsonStr :: Maybe SvgJSON
    case jsonObj of
        Nothing -> return $ toResponse ("Error" :: String)
        Just (SvgJSON texts shapes paths) -> do
            _ <- liftIO $ runDb $ insertGraph nameStr texts shapes paths
            return $ toResponse ("Success" :: String)
    where
        insertGraph :: T.Text -> [Text] -> [Shape] -> [Path] -> SqlPersistM ()
        insertGraph nameStr_ texts shapes paths = do
            gId <- insert $ Graph nameStr_ 256 256 False
            insertMany_ $ map (\text -> text {textGraph = gId}) texts
            insertMany_ $ map (\shape -> shape {shapeGraph = gId}) shapes
            insertMany_ $ map (\path -> path {pathGraph = gId}) paths
