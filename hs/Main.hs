module Main where

import System.Environment (getArgs)
import Data.Map.Strict as Map

-- internal dependencies
import Server (runServer)
import Database.Database (setupDatabase)
import Svg.Parser (parsePrebuiltSvgs)

taskNamesToTasks :: Map.Map String (IO ())
taskNamesToTasks = Map.fromList [("server", runServer),
                                 ("database", setupDatabase),
                                 ("graphs", parsePrebuiltSvgs)]

main :: IO ()
main = do
    args <- getArgs
    let command = if length args == 0 then "server" else args !! 0

    -- extract and perform the task we want to run
    taskNamesToTasks ! command
