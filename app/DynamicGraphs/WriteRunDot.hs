module DynamicGraphs.WriteRunDot where

import           Control.Monad   (forM_)
import           Data.GraphViz
import           System.FilePath (FilePath, combine, normalise)
import           DynamicGraphs.GraphGenerator (coursesToPrereqGraph)

doDots :: PrintDotRepr dg n => [(FilePath, dg n)] -> IO ()
doDots cases = do
  forM_ cases createImage
  putStrLn "Look in graphs/gen to see the created graphs"

generatePrereqsForCourses :: (FilePath, [String]) -> IO ()
generatePrereqsForCourses (output, courses) = do
  graph <- coursesToPrereqGraph courses
  _ <- createImage (output, graph)
  putStrLn $ "Generated prerequisite graph for " ++ show courses

createImage :: PrintDotRepr dg n => (FilePath, dg n) -> IO FilePath
createImage (n, g) = createImageInDir (normalise "graphs/gen") n Svg g

-- Here runGraphvizCommand Dot creates the final graph given the input DotGraph object g and connects it
-- with the file path(by combining directory d and filename n) to make the final graph in the required directory.
createImageInDir :: PrintDotRepr dg n => FilePath -> FilePath -> GraphvizOutput -> dg n -> IO FilePath
createImageInDir d n o g = Data.GraphViz.addExtension (runGraphvizCommand Dot g) o (combine d n)
