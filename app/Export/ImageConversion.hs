{-|
    Module      : Export.ImageConversion
    Description : Includes functions for converting SVG files to PNG.
-}
module Export.ImageConversion
    (createImageFile, removeFile) where

import System.Process (createProcess, shell, waitForProcess, ProcessHandle)
import GHC.IO.Handle.Types
import Turtle.Prelude (rm)
import Filesystem.Path.CurrentOS as Path
import Data.List.Split (splitOn)

-- | Opens a new process to convert an SVG (inName) to a PNG (outName)
-- Note: hGetContents can be used to read Handles. Useful when trying to read from
-- stdout.
createImageFile :: String -> String -> IO ()
createImageFile inName outName =  do
    (_, _, _, pid) <- convertToImage inName outName
    putStrLn "Waiting for process..."
    _ <- waitForProcess pid
    putStrLn "Process Complete"

-- | Converts an SVG file to a PNG file. Note that image magik's 'convert' command
-- can take in file descriptors.
convertToImage :: String -> String -> IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)
convertToImage inName outName =
    createProcess $ shell $ "magick convert " ++ inName ++ " " ++ outName

-- | Removes a file.
removeFile :: String -> IO ()
removeFile name = mapM_ (rm . Path.decodeString) (splitOn " " name)
