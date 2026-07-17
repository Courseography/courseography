module Models.Graph (getGraph, insertGraph, insertElements, deleteExistingGraph, parseGraphComponentsJSON) where

import Config (runDb)
import Data.Aeson (Value, decode, object, toJSON, (.:))
import Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T (Text)
import Database.DataType (ShapeType (BoolNode, Hybrid, Node))
import Database.Persist.Sqlite (
    Entity,
    PersistEntity,
    PersistValue (PersistInt64),
    SqlPersistM,
    deleteWhere,
    entityKey,
    entityVal,
    insert,
    insertMany_,
    insert_,
    keyToValues,
    selectFirst,
    selectList,
    (<-.),
    (==.),
 )
import Database.Tables (
    EntityField (GraphId, GraphTitle, PathGraph, ShapeGraph, ShapeType_, TextGraph),
    Graph (Graph, graphHeight, graphWidth),
    Key,
    Path (pathGraph),
    Shape (shapeGraph),
    Text (textGraph),
 )
import Svg.Builder (buildEllipses, buildPath, buildRect)
import Util.Helpers

getGraph :: T.Text -> IO (Maybe Value)
getGraph graphName = runDb $ do
    graphEnt :: (Maybe (Entity Graph)) <- selectFirst [GraphTitle ==. graphName] []
    case graphEnt of
        Nothing -> return Nothing
        Just graph -> do
            let gId = entityKey graph
            sqlTexts :: [Entity Text] <- selectList [TextGraph ==. gId] []
            sqlRects :: [Entity Shape] <-
                selectList
                    [ ShapeType_ <-. [Node, Hybrid]
                    , ShapeGraph ==. gId
                    ]
                    []
            sqlEllipses :: [Entity Shape] <-
                selectList
                    [ ShapeType_ ==. BoolNode
                    , ShapeGraph ==. gId
                    ]
                    []
            sqlPaths :: [Entity Path] <- selectList [PathGraph ==. gId] []

            let
                keyAsInt :: PersistEntity a => Entity a -> Integer
                keyAsInt = fromIntegral . (\(PersistInt64 x) -> x) . safeHead (PersistInt64 0) . keyToValues . entityKey

                graphtexts = map entityVal sqlTexts
                rects =
                    zipWith
                        (buildRect graphtexts)
                        (map entityVal sqlRects)
                        (map keyAsInt sqlRects)
                ellipses =
                    zipWith
                        (buildEllipses graphtexts)
                        (map entityVal sqlEllipses)
                        (map keyAsInt sqlEllipses)
                graphpaths =
                    zipWith
                        (buildPath rects ellipses)
                        (map entityVal sqlPaths)
                        (map keyAsInt sqlPaths)

                response =
                    object
                        [ ("texts", toJSON graphtexts)
                        , ("shapes", toJSON $ rects ++ ellipses)
                        , ("paths", toJSON graphpaths)
                        , ("width", toJSON $ graphWidth $ entityVal graph)
                        , ("height", toJSON $ graphHeight $ entityVal graph)
                        ]

            return (Just response)

-- | Insert a new graph into the database, given its SVG JSON.
-- | Return Nothing.
insertGraph ::
    -- | The title of the graph being inserted.
    T.Text ->
    -- | The parsed JSON data of the inserted graph.
    ([Text], [Shape], [Path]) ->
    -- | Return Nothing.
    SqlPersistM ()
insertGraph nameStr_ (texts, shapes, paths) = do
    gId <- insert $ Graph nameStr_ 256 256 False
    insertMany_ $ map (\text -> text{textGraph = gId}) texts
    insertMany_ $ map (\shape -> shape{shapeGraph = gId}) shapes
    insertMany_ $ map (\path -> path{pathGraph = gId}) paths

-- | Insert graph components into the database.
insertElements :: ([Path], [Shape], [Text]) -> SqlPersistM ()
insertElements (paths, shapes, texts) = do
    mapM_ insert_ shapes
    mapM_ insert_ paths
    mapM_ insert_ texts

-- | Delete the graph with the given name from the database, if it exists.
deleteExistingGraph :: T.Text -> SqlPersistM ()
deleteExistingGraph graphName = do
    graphEnt :: (Maybe (Entity Graph)) <- selectFirst [GraphTitle ==. graphName] []
    case graphEnt of
        Just graph -> do
            let gId = entityKey graph
            deleteGraph gId
        Nothing -> pure ()

-- | Delete a graph with the given graph ID from the database.
deleteGraph :: Key Graph -> SqlPersistM ()
deleteGraph gId = do
    deleteWhere [TextGraph ==. gId]
    deleteWhere [ShapeGraph ==. gId]
    deleteWhere [PathGraph ==. gId]
    deleteWhere [GraphId ==. gId]

-- | Parse the JSON representation of a graph into its texts, shapes, and paths components.
parseGraphComponentsJSON :: L.ByteString -> Maybe ([Text], [Shape], [Path])
parseGraphComponentsJSON jsonStr = do
    obj <- decode jsonStr
    parseMaybe
        ( \o -> do
            texts <- o .: "texts"
            shapes <- o .: "shapes"
            paths <- o .: "paths"
            return (texts, shapes, paths)
        )
        obj
