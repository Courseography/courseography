module DynamicGraphs.WriteRunDot where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.GraphViz hiding (Str)
import System.FilePath (FilePath, combine, normalise)
import System.Directory (createDirectoryIfMissing)
import DynamicGraphs.GraphGenerator (coursesToPrereqGraph, coursesToPrereqGraphExcluding)
import Happstack.Server (ServerPart, lookBS)
import Happstack.Server.SimpleHTTP (Response)
import qualified Data.Text as T
import qualified Data.ByteString as B
import Svg.Parser (parseDynamicSvg)
import Data.Text.Encoding (decodeUtf8)
import Data.Hash.MD5 (Str(Str), md5s)
import Database.CourseQueries (getGraph)
import Data.Aeson (decode)
import Data.Maybe (fromMaybe)

doDots :: PrintDotRepr dg n => [(FilePath, dg n)] -> IO ()
doDots cases = do
  forM_ cases createImage
  putStrLn "Look in graphs/gen to see the created graphs"

generatePrereqsForCourses :: (FilePath, [String]) -> IO ()
generatePrereqsForCourses (output, courses) = do
  graph <- coursesToPrereqGraph courses
  _ <- createImage (output, graph)
  putStrLn $ "Generated prerequisite graph for "
    ++ show courses
    ++ " in graphs/gen"

findPrereqsResponse :: ServerPart Response
findPrereqsResponse = do
    takenStr <- lookBS "taken"
    coursesStr <- lookBS "courses"
    let taken = fromMaybe [] $ decode takenStr
        courses = fromMaybe [] $ decode coursesStr
    liftIO $ generatePrereqResponse taken courses

generatePrereqResponse :: [String] -> [String] -> IO Response
generatePrereqResponse taken courses = do
  cached <- getGraph graphHash
  case cached of
    Just cachedGraph -> return cachedGraph
    Nothing -> do
      graph <- coursesToPrereqGraphExcluding taken courses
      bString <- graphToByteString graph
      -- Parse the generated SVG and store it in the database.
      parseDynamicSvg graphHash $ decodeUtf8 bString
      storedGraph <- getGraph graphHash
      return $ fromMaybe graphNotFound storedGraph
  where
    -- Uniquely identify the graph in the database.
    graphHash :: T.Text
    graphHash = (T.pack . ("gen_" ++) . md5s . Str . show) (taken, courses)
    graphNotFound = error "Graph should have been generated but was not found"

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
