{-|
Description: Entry point for Courseography.

This module implements a 'main' method which takes a command-line
argument and executes the corresponding IO action.

If no argument is provided, the default action is 'server', which
starts the server.
-}
module Main where

import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

-- internal dependencies
import Database.Database (runDatabase, setupDatabase)
import Server (runServer)
import Svg.Parser (parsePrebuiltSvgs)
import Util.Documentation (generateDocs)

-- dynamic graph generation
import DynamicGraphs.WriteRunDot (generatePrereqsForCourses)

-- | A map of command-line arguments to their corresponding IO actions.
taskMap :: Map.Map String ([String] -> IO ())
taskMap = Map.fromList [
    ("server", const runServer),
    ("database", const runDatabase),
    ("graphs", const parsePrebuiltSvgs),
    ("docs", const generateDocs),
    ("generate", generate),
    ("database-setup", const setupDatabase)]

-- | Courseography entry point.
main :: IO ()
main = do
    args <- getArgs
    let (taskName, rest) =
            case args of
                [] -> ("server", [])
                (command : remaining) -> (command, remaining)
    fromMaybe putUsage (Map.lookup taskName taskMap) rest


-- | Print usage message to user (when main gets an incorrect argument).
putUsage :: [String] -> IO ()
putUsage _ = hPutStrLn stderr usageMsg
    where
        taskNames = Map.keys taskMap
        usageMsg = "Unrecognized argument. Available arguments:\n" ++
                   intercalate "\n" taskNames

generate :: [String] -> IO ()
generate (name : courses) = generatePrereqsForCourses (name, courses)
generate _ = hPutStrLn
    stderr
    "Generate Usage: generate <filename> <course1> <course2> ..."
