module ImageConversion where

import System.Process
import GHC.IO.Handle.Types
import Control.Monad.IO.Class  (liftIO)
import GHC.IO.Exception

-- | Converts an SVG file to a PNG file. Note that image magik's 'convert' command
-- can take in file descriptors.
convertToImage :: String -> String -> IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)

convertToImage inName outName = createProcess $ CreateProcess
                                  (ShellCommand $ "convert " ++
                                                  inName ++
                                                  " " ++
                                                  outName
                                  )
                                  Nothing
                                  Nothing
                                  CreatePipe
                                  CreatePipe
                                  CreatePipe
                                  False
                                  False
                                  False
-- | Removes a file.
removeImage :: String -> IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)

removeImage name = createProcess $ CreateProcess
                                  (ShellCommand $ "rm " ++ name)
                                  Nothing
                                  Nothing
                                  Inherit
                                  CreatePipe
                                  CreatePipe
                                  False
                                  False
                                  False

-- Note: hGetContents can be used to read Handles. Useful when trying to read from
-- stdout.
createImageFile :: String -> String -> IO ()
createImageFile inName outName =
    do (inp, out, err, pid) <- convertToImage inName outName
       liftIO $ print "Waiting for process..."
       liftIO $ waitForProcess pid
       liftIO $ print "Process Complete"
