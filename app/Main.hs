{-|
Description: Entry point for Courseography.

This module implements a 'main' method which takes a command-line
argument and executes the corresponding IO action.

If no argument is provided, the default action is 'server', which
starts the server.
-}
module Main where

import System.IO (stderr, hPutStrLn)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map

-- internal dependencies
import Server (runServer)
import Database.Database (setupDatabase)
import Svg.Parser (parsePrebuiltSvgs)
import Css.Compiler (compileCSS)
import Util.Documentation (generateDocs)

-- dynamic graph generation
import DynamicGraphs.WriteRunDot(generatePrereqsForCourse)

-- | A map of command-line arguments to their corresponding IO actions.
taskMap :: Map.Map String (IO ())
taskMap = Map.fromList [
    ("server", runServer),
    ("database", setupDatabase),
    ("graphs", parsePrebuiltSvgs),
    ("css", compileCSS),
    ("docs", generateDocs),
    ("generate", generatePrereqsForCourse ("CSC401H1", "CSC401H1"))]

-- | Courseography entry point.
main :: IO ()
main = do
    args <- getArgs
    let taskName = if null args then "server" else head args
    fromMaybe putUsage (Map.lookup taskName taskMap)


-- | Print usage message to user (when main gets an incorrect argument).
putUsage :: IO ()
putUsage = hPutStrLn stderr usageMsg
    where
        taskNames = Map.keys taskMap
        usageMsg = "Unrecognized argument. Available arguments:\n" ++
                   intercalate "\n" taskNames
