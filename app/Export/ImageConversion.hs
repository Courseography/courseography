{-|
    Module      : Export.ImageConversion
    Description : Includes functions for converting SVG files to PNG.
-}
module Export.ImageConversion
    (createImageFile, withImageFile) where

import GHC.IO.Handle.Types
import System.IO (hClose)
import System.Process (ProcessHandle, StdStream (CreatePipe), createProcess, proc, shell, std_in,
                       waitForProcess)

-- | Opens a new process to convert an SVG read from stdin to a PNG (outName)
withImageFile :: String -> (Handle -> IO ()) -> IO ()
withImageFile outName writeAction = do
    (Just hin, _, _, pid) <- createProcess (proc "magick" ["svg:-", outName]) { std_in = CreatePipe }
    writeAction hin
    hClose hin
    putStrLn "Waiting for process..."
    _ <- waitForProcess pid
    putStrLn "Process Complete"

-- | Opens a new process to convert an SVG (inName) to a PNG (outName)
-- Note: hGetContents can be used to read Handles. Useful when trying to read from
-- stdout.
createImageFile :: String -> String -> IO ()
createImageFile inName outName =  do
    (_, _, _, pid) <- convertToImage inName outName
    putStrLn "Waiting for process..."
    _ <- waitForProcess pid
    putStrLn "Process Complete"

-- | Converts an SVG file to a PNG file. Note that ImageMagick's 'magick' command
-- can take in file descriptors.
convertToImage :: String -> String -> IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)
convertToImage inName outName =
    createProcess $ shell $ "magick " ++ inName ++ " " ++ outName
