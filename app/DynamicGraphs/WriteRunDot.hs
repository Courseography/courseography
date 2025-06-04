module DynamicGraphs.WriteRunDot where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.GraphViz hiding (Str)
import Data.Hash.MD5 (Str (Str), md5s)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Models.Graph (getGraph)
import DynamicGraphs.GraphGenerator (coursesToPrereqGraph, coursesToPrereqGraphExcluding,
                                     graphProfileHash)
import DynamicGraphs.GraphOptions (CourseGraphOptions (..), GraphOptions (..))
import Happstack.Server (ServerPart, askRq)
import Happstack.Server.SimpleHTTP (Response)
import Happstack.Server.Types (takeRequestBody, unBody)
import Svg.Parser (parseDynamicSvg)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (combine, normalise)
import Util.Happstack (createJSONResponse)

doDots :: PrintDotRepr dg n => [(FilePath, dg n)] -> IO ()
doDots cases = do
  forM_ cases createImage
  putStrLn "Look in graphs/gen to see the created graphs"

generatePrereqsForCourses :: (FilePath, [String]) -> IO ()
generatePrereqsForCourses (output, rootCourses) = do
  graph <- coursesToPrereqGraph rootCourses
  _ <- createImage (output, graph)
  putStrLn $ "Generated prerequisite graph for "
    ++ show rootCourses
    ++ " in graphs/gen"

getBody :: ServerPart L.ByteString
getBody = do
    req  <- askRq
    body <- liftIO $ takeRequestBody req
    case body of
        Just rqbody -> return . unBody $ rqbody
        Nothing     -> return ""

generateAndSavePrereqResponse :: CourseGraphOptions -> IO Response
generateAndSavePrereqResponse coursesOptions = do
  cached <- getGraph graphHash
  case cached of
    Just cachedGraph -> return $ createJSONResponse cachedGraph
    Nothing -> do
      graph <- coursesToPrereqGraphExcluding (courses coursesOptions) (graphOptions coursesOptions)
      bString <- graphToByteString graph
      -- Parse the generated SVG and store it in the database.
      parseDynamicSvg graphHash $ decodeUtf8 bString
      storedGraph <- getGraph graphHash
      return $ createJSONResponse $ fromMaybe graphNotFound storedGraph
  where
    graphHash :: T.Text
    graphHash = hash coursesOptions
    graphNotFound = error "Graph should have been generated but was not found"

-- | Hash function to uniquely identify the graph layout.
hash :: CourseGraphOptions -> T.Text
hash coursesOptions = hashFunction (key, graphProfileHash)
  where key = coursesOptions {
          courses = sort $ courses coursesOptions,
          graphOptions = options {
            taken = sort $ taken options,
            departments = sort $ departments options,
            distribution = sort $ distribution options,
            location = sort $ location options,
            courseNumPrefix = sort $ courseNumPrefix options
            }
          }
          where options = graphOptions coursesOptions
        hashFunction :: (CourseGraphOptions, String) -> T.Text
        hashFunction = T.pack . ("graph_" ++) . md5s . Str . show

graphToByteString :: PrintDotRepr dg n => dg n -> IO B.ByteString
graphToByteString graph = graphvizWithHandle Dot graph Svg B.hGetContents

createImage :: PrintDotRepr dg n => (FilePath, dg n) -> IO FilePath
createImage (n, g) = do
  _ <- createDirectoryIfMissing False filepath
  createImageInDir filepath n Svg g
  where
    filepath = normalise "graphs/gen"

-- | Here runGraphvizCommand Dot creates the final graph given the input DotGraph object g and connects it
-- with the file path(by combining directory d and filename n) to make the final graph in the required directory.
createImageInDir :: PrintDotRepr dg n
  => FilePath -- ^ directory to write in
  -> FilePath -- ^ filename
  -> GraphvizOutput -- ^ filetype to write in
  -> dg n -- ^ graph to draw
  -> IO FilePath
createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)
