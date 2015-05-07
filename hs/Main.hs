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

taskNamesToTasks :: Map.Map String (IO ())
taskNamesToTasks = Map.fromList [("server", runServer),
                                 ("database", setupDatabase),
                                 ("graphs", parsePrebuiltSvgs)]

getTask :: String -> Maybe (IO ())
getTask taskName = Map.lookup taskName taskNamesToTasks

main :: IO ()
main = do
    args <- getArgs
    let taskName = if null args then "server" else args !! 0

    -- extract and perform the task we want to run
    let taskNames = Map.keys taskNamesToTasks
    let availableTasksStr = "[" ++ (intercalate ", " taskNames) ++ "]"
    let lookupFailedMsg = "No task named " ++ taskName ++
                          ", available tasks are " ++ availableTasksStr

    fromMaybe (hPutStrLn stderr lookupFailedMsg) (getTask taskName)
