module Main where

import System.IO (stderr, hPutStrLn)
import System.Environment (getArgs)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Css.CssGen (generateCSS)
import qualified Data.Map.Strict as Map

-- internal dependencies
import Server (runServer)
import Database.Database (setupDatabase)
import Svg.Parser (parsePrebuiltSvgs)

taskNamesToTasks :: Map.Map String (IO ())
taskNamesToTasks = Map.fromList [
    ("server", runServer),
    ("database", setupDatabase),
    ("graphs", parsePrebuiltSvgs),
    ("css", generateCSS)]

getTask :: String -> Maybe (IO ())
getTask taskName = Map.lookup taskName taskNamesToTasks

main :: IO ()
main = do
    args <- getArgs
    let taskName = if null args then "server" else head args

    -- extract and perform the task we want to run
    let taskNames = Map.keys taskNamesToTasks
    let availableTasksStr = "[" ++ (intercalate ", " taskNames) ++ "]"
    let lookupFailedMsg = "No task named " ++ taskName ++
                          ", available tasks are " ++ availableTasksStr

    fromMaybe (hPutStrLn stderr lookupFailedMsg) (getTask taskName)
