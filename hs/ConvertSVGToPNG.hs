module ConvertSVGToPNG where

import System.Process
import GHC.IO.Handle.Types
import Control.Monad.IO.Class  (liftIO)
import GHC.IO.Exception

-- | Converts an SVG file to a PNG file. Note that image magik's 'convert' command
-- can take in file descriptors.
convertSVGToPNG :: String -> String -> IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)

convertSVGToPNG inName outName = createProcess $ CreateProcess
                                  (ShellCommand $ "convert Testfile2.svg INSERT_ID-graph.png"
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
removePNG :: String -> IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)

removePNG name = createProcess $ CreateProcess
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
createPNGFile :: String -> IO ExitCode
createPNGFile uniqueName = do (inp, out, err, pid) <- convertSVGToPNG uniqueName "../res/graphs/graph_regions.svg"
                              liftIO $ print "Waiting for process..."
                              liftIO $ waitForProcess pid
