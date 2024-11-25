{-|
    Module      : Export.ImageConversion
    Description : Includes functions for converting SVG files to PNG.
-}
module Export.ImageConversion
    (createImageFile) where

import GHC.IO.Handle.Types
import System.Process (ProcessHandle, createProcess, shell, waitForProcess)

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
    createProcess $ shell $ "magick " ++ inName ++ " " ++ outName
