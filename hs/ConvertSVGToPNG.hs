module ConvertSVGToPNG where

import System.Process
import GHC.IO.Handle.Types
import Control.Monad.IO.Class  (liftIO)
import GHC.IO.Exception
import System.IO (hFlush, hIsEOF, hShow, hReady, hGetContents)

convertSVGToPNG :: String -> String -> IO
                     (Maybe Handle,
                      Maybe Handle,
                      Maybe Handle,
                      ProcessHandle)

convertSVGToPNG inName outName = createProcess $ CreateProcess
                                  (ShellCommand $ "convert ../res/graphs/graph_regions.svg INSERT_ID-graph.png"
                                   )
                                  Nothing
                                  Nothing
                                  CreatePipe
                                  CreatePipe
                                  CreatePipe
                                  False
                                  False
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

-- Note: hGetContents can be used to read Handles.
createPNGFile :: String -> IO ExitCode
createPNGFile uniqueName = do (inp, out, err, pid) <- convertSVGToPNG uniqueName "../res/graphs/graph_regions.svg"
                              liftIO $ print "Waiting for process..."
                              liftIO $ waitForProcess pid
