module DynamicGraphs.WriteRunDot where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.GraphViz hiding (Str)
import System.FilePath (FilePath, combine, normalise)
import System.Directory (createDirectoryIfMissing)
import DynamicGraphs.GraphGenerator (coursesToPrereqGraph, coursesToPrereqGraphExcluding, graphProfileHash)
import Happstack.Server (ServerPart, askRq)
import Happstack.Server.Types (takeRequestBody, unBody)
import Happstack.Server.SimpleHTTP (Response)
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Svg.Parser (parseDynamicSvg)
import Data.Text.Encoding (decodeUtf8)
import Data.Hash.MD5 (Str(Str), md5s)
import Database.CourseQueries (getGraph)
import Data.Aeson (decode)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (sort)
import DynamicGraphs.GraphOptions (GraphOptions(..), CourseGraphOptions(..))

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

findAndSavePrereqsResponse :: ServerPart Response
findAndSavePrereqsResponse = do
    body <- getBody
    let coursesOptions :: CourseGraphOptions = fromJust $ decode body
    liftIO $ generateAndSavePrereqResponse coursesOptions

generateAndSavePrereqResponse :: CourseGraphOptions -> IO Response
generateAndSavePrereqResponse coursesOptions = do
  cached <- getGraph graphHash
  case cached of
    Just cachedGraph -> return cachedGraph
    Nothing -> do
      graph <- coursesToPrereqGraphExcluding (courses coursesOptions) (graphOptions coursesOptions)
      bString <- graphToByteString graph
      -- Parse the generated SVG and store it in the database.
      parseDynamicSvg graphHash $ decodeUtf8 bString
      storedGraph <- getGraph graphHash
      return $ fromMaybe graphNotFound storedGraph
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
