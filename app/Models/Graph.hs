module Models.Graph
    (getGraph,
    insertGraph) where

import Config (runDb)
import Data.Aeson (Value, object, toJSON)
import qualified Data.Text as T (Text)
import Database.DataType (ShapeType (BoolNode, Hybrid, Node))
import Database.Persist.Sqlite (Entity, PersistEntity, PersistValue (PersistInt64), SqlPersistM,
                                entityKey, entityVal, insert, insertMany_, keyToValues, selectFirst,
                                selectList, (<-.), (==.))
import Database.Tables hiding (paths, shapes, texts)
import Svg.Builder (buildEllipses, buildPath, buildRect)
import Util.Helpers

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
                keyAsInt = fromIntegral . (\(PersistInt64 x) -> x) . safeHead (PersistInt64 0) . keyToValues . entityKey

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

                response = object [
                        ("texts", toJSON graphtexts),
                        ("shapes", toJSON $ rects ++ ellipses),
                        ("paths", toJSON graphpaths),
                        ("width", toJSON $ graphWidth $ entityVal graph),
                        ("height", toJSON $ graphHeight $ entityVal graph)
                    ]

            return (Just response)

insertGraph :: T.Text -> SvgJSON -> SqlPersistM ()
insertGraph nameStr_ (SvgJSON texts shapes paths) = do
    gId <- insert $ Graph nameStr_ 256 256 False
    insertMany_ $ map (\text -> text {textGraph = gId}) texts
    insertMany_ $ map (\shape -> shape {shapeGraph = gId}) shapes
    insertMany_ $ map (\path -> path {pathGraph = gId}) paths
